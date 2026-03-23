{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Database.PSQL.Select
  ( count
  , count_
  , countInRaw
  , countCol

  , selectRaw
  , selectRaw_

  , selectAllRaw
  , selectAllRaw_

  , select
  , select_

  , selectAll
  , selectAll_

  , selectOnly
  , selectOnly_

  , selectAllOnly
  , selectAllOnly_

  , selectOne
  , selectOneOnly

  , selectInRaw
  , selectIn
  , selectInOnly
  ) where


import           Data.Int                           (Int64)
import           Data.Char                          (isAlphaNum, isLetter, isSpace)
import           Data.List                          (isPrefixOf)
import           Data.Maybe                         (listToMaybe)
import           Database.PostgreSQL.Simple         (Only (..))
import           Database.PostgreSQL.Simple.FromRow (FromRow (..))
import           Database.PostgreSQL.Simple.ToField (ToField (..))
import           Database.PostgreSQL.Simple.ToRow   (ToRow (..))
import           Database.PSQL.Gen                  (genAnd, genIn, genSelect)
import           Database.PSQL.Types.Column         (Column (..), Columns)
import           Database.PSQL.Types.GroupBy        (GroupBy, groupNone)
import           Database.PSQL.Types.Page           (Page, pageNone, pageOne)
import           Database.PSQL.Types.PSQL           (PSQL, query, query_)
import           Database.PSQL.Types.TableName      (TableName)
import           Database.PSQL.Util                 (getOnlyDefault)

selectRaw :: (ToRow a, FromRow b) => TableName -> Columns -> String -> a -> Page -> GroupBy -> PSQL [b]
selectRaw tn cols partSql a p g
  | expectedCount == length rowParams = query sql a
  | otherwise = error "selectRaw: placeholder count does not match params"
  where sql = genSelect tn cols partSql p g
        rowParams = toRow a
        expectedCount = countPlaceholders partSql

selectRaw_ :: FromRow b => TableName -> Columns -> String -> Page -> GroupBy -> PSQL [b]
selectRaw_ tn cols partSql p g
  | countPlaceholders partSql == 0 = query_ sql
  | otherwise = error "selectRaw_: placeholders present but no params provided"
  where sql = genSelect tn cols partSql p g

selectAllRaw :: (ToRow a, FromRow b) => TableName -> Columns -> String -> a -> GroupBy -> PSQL [b]
selectAllRaw tn cols partSql a = selectRaw tn cols partSql a pageNone

selectAllRaw_ :: FromRow b => TableName -> Columns -> String -> GroupBy -> PSQL [b]
selectAllRaw_ tn cols partSql = selectRaw_ tn cols partSql pageNone

selectInRaw :: (ToField a, ToRow r, FromRow b) => TableName -> Columns -> Column -> [a] -> String -> r -> Page -> GroupBy -> PSQL [b]
selectInRaw tn cols col xs partSql r
  | expectedCount == length ys = selectRaw tn cols ids ys
  | otherwise = error "selectInRaw: placeholder count does not match params"
  where (inSql, ixs) = genIn col xs
        ids = inSql `genAnd` partSql
        rowParams = toRow r
        ys
          | all isSpace partSql =
            case rowParams of
              [] -> ixs
              _  -> error "selectInRaw: non-empty params with blank partSql"
          | otherwise = ixs ++ rowParams
        expectedCount = countPlaceholders ids

countPlaceholders :: String -> Int
countPlaceholders = goNormal False 0
  where
    goNormal :: Bool -> Int -> String -> Int
    goNormal _ n []                = n
    goNormal _ n ('?':'?':rest)    = goNormal False n rest
    goNormal _ n ('?':rest)        = goNormal False (n + 1) rest
    goNormal _ n ('\'':rest)       = goSingleQuoted n rest
    goNormal _ n ('"':rest)        = goDoubleQuoted n rest
    goNormal prevIsIdent n ('$':rest)
      | not prevIsIdent =
          case parseDollarTag rest of
            Just (tag, body) -> goDollarQuoted n tag body
            Nothing          -> goNormal True n rest
      | otherwise = goNormal True n rest
    goNormal _ n ('-':'-':rest)    = goLineComment n rest
    goNormal _ n ('/':'*':rest)    = goBlockComment n 1 rest
    goNormal _ n (c:rest)          = goNormal (isIdentChar c) n rest

    goSingleQuoted :: Int -> String -> Int
    goSingleQuoted n []                = n
    goSingleQuoted n ('\'':'\'':rest)  = goSingleQuoted n rest
    goSingleQuoted n ('\'':rest)       = goNormal False n rest
    goSingleQuoted n (_:rest)          = goSingleQuoted n rest

    goDoubleQuoted :: Int -> String -> Int
    goDoubleQuoted n []              = n
    goDoubleQuoted n ('"':'"':rest)  = goDoubleQuoted n rest
    goDoubleQuoted n ('"':rest)      = goNormal False n rest
    goDoubleQuoted n (_:rest)        = goDoubleQuoted n rest

    goLineComment :: Int -> String -> Int
    goLineComment n []           = n
    goLineComment n ('\n':rest)  = goNormal False n rest
    goLineComment n (_:rest)     = goLineComment n rest

    goBlockComment :: Int -> Int -> String -> Int
    goBlockComment n _ []                    = n
    goBlockComment n d ('/':'*':rest)        = goBlockComment n (d + 1) rest
    goBlockComment n 1 ('*':'/':rest)        = goNormal False n rest
    goBlockComment n d ('*':'/':rest)        = goBlockComment n (d - 1) rest
    goBlockComment n d (_:rest)              = goBlockComment n d rest

    goDollarQuoted :: Int -> String -> String -> Int
    goDollarQuoted n tag = go
      where delimiter = '$' : tag ++ "$"
            delimiterLen = length delimiter

            go [] = n
            go xs
              | delimiter `isPrefixOf` xs = goNormal False n (drop delimiterLen xs)
              | otherwise =
                  case xs of
                    _:rest -> go rest

    parseDollarTag :: String -> Maybe (String, String)
    parseDollarTag xs =
      case break (== '$') xs of
        (tag, '$':rest)
          | isValidDollarTag tag -> Just (tag, rest)
        _ -> Nothing

    isValidDollarTag :: String -> Bool
    isValidDollarTag "" = True
    isValidDollarTag (c:cs) =
      (isLetter c || c == '_') && all isDollarTagChar cs

    isDollarTagChar :: Char -> Bool
    isDollarTagChar c = isAlphaNum c || c == '_'

    isIdentChar :: Char -> Bool
    isIdentChar c = isAlphaNum c || c == '_' || c == '$'

select :: (ToRow a, FromRow b) => TableName -> Columns -> String -> a -> Page -> PSQL [b]
select tn cols partSql a p = selectRaw tn cols partSql a p groupNone

select_ :: FromRow b => TableName -> Columns -> Page -> PSQL [b]
select_ tn cols p = selectRaw_ tn cols "" p groupNone

selectAll :: (ToRow a, FromRow b) => TableName -> Columns -> String -> a -> PSQL [b]
selectAll tn cols partSql a = selectAllRaw tn cols partSql a groupNone

selectAll_ :: FromRow b => TableName -> Columns -> PSQL [b]
selectAll_ tn cols = selectAllRaw_ tn cols "" groupNone

selectIn :: (ToField a, FromRow b) => TableName -> Columns -> Column -> [a] -> PSQL [b]
selectIn tn cols col xs = selectInRaw tn cols col xs "" () pageNone groupNone

selectOnly :: (ToRow a, FromRow (Only b)) => TableName -> Column -> String -> a -> Page -> PSQL [b]
selectOnly tn col partSql a p =
  map fromOnly <$> select tn [col] partSql a p

selectInOnly :: (ToField a, ToRow r, FromRow (Only b)) => TableName -> Column -> Column -> [a] -> String -> r -> Page -> PSQL [b]
selectInOnly tn outCol col xs partSql a p =
  map fromOnly <$> selectInRaw tn [outCol] col xs partSql a p groupNone

selectOnly_ :: FromRow (Only b) => TableName -> Column -> Page -> PSQL [b]
selectOnly_ tn col p =
  map fromOnly <$> select_ tn [col] p

selectAllOnly :: (ToRow a, FromRow (Only b)) => TableName -> Column -> String -> a -> PSQL [b]
selectAllOnly tn col partSql a =
  map fromOnly <$> selectAll tn [col] partSql a

selectAllOnly_ :: FromRow (Only b) => TableName -> Column -> PSQL [b]
selectAllOnly_ tn col = map fromOnly <$> selectAll_ tn [col]

selectOne :: (ToRow a, FromRow b) => TableName -> Columns -> String -> a -> PSQL (Maybe b)
selectOne tn cols partSql a = listToMaybe <$> select tn cols partSql a pageOne

selectOneOnly :: (ToRow a, FromRow (Only b)) => TableName -> Column -> String -> a -> PSQL (Maybe b)
selectOneOnly tn col partSql a =
  fmap fromOnly <$> selectOne tn [col] partSql a

countCol :: ToRow a => TableName -> Column -> String -> a -> PSQL Int64
countCol tn (Column c) partSql a = getOnlyDefault 0 <$> selectAll tn [col] partSql a
  where col = Column $ "count(" ++ c ++")"

count :: ToRow a => TableName -> String -> a -> PSQL Int64
count tn partSql a = getOnlyDefault 0 <$> selectAll tn ["count(*)"] partSql a

count_ :: TableName -> PSQL Int64
count_ tn = getOnlyDefault 0 <$> selectAll_ tn ["count(*)"]

countInRaw :: (ToField a, ToRow r) => TableName -> Column -> [a] -> String -> r -> PSQL Int64
countInRaw tn col xs partSql a =
  getOnlyDefault 0 <$> selectInRaw tn ["count(*)"] col xs partSql a pageOne groupNone
