{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Database.PSQL.Select
  ( count
  , count_

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
  ) where


import           Data.Int                           (Int64)
import           Data.Maybe                         (listToMaybe)
import           Database.PostgreSQL.Simple         (Only (..))
import           Database.PostgreSQL.Simple.FromRow (FromRow (..))
import           Database.PostgreSQL.Simple.ToField (ToField (..))
import           Database.PostgreSQL.Simple.ToRow   (ToRow (..))
import           Database.PSQL.Gen                  (genSelect)
import           Database.PSQL.Types.Column         (Column (..), Columns,
                                                     columnsToString)
import           Database.PSQL.Types.From           (From (..))
import           Database.PSQL.Types.GroupBy        (GroupBy, groupNone)
import           Database.PSQL.Types.OrderBy        (OrderBy, orderNone)
import           Database.PSQL.Types.PSQL           (PSQL, query, query_)
import           Database.PSQL.Types.Size           (Size (..))
import           Database.PSQL.Types.TableName      (TableName)
import           Database.PSQL.Util                 (getOnlyDefault)

selectRaw :: (ToRow a, FromRow b) => TableName -> Columns -> String -> a -> From -> Size -> GroupBy -> OrderBy -> PSQL [b]
selectRaw tn cols partSql a from size g o = query sql a
  where sql = genSelect tn cols partSql from size g o

selectRaw_ :: FromRow b => TableName -> Columns -> String -> From -> Size -> GroupBy -> OrderBy -> PSQL [b]
selectRaw_ tn cols partSql from size g o = query_ sql
  where sql = genSelect tn cols partSql from size g o

selectAllRaw :: (ToRow a, FromRow b) => TableName -> Columns -> String -> a -> GroupBy -> OrderBy -> PSQL [b]
selectAllRaw tn cols partSql a g o = selectRaw tn cols partSql a 0 0 g o

selectAllRaw_ :: FromRow b => TableName -> Columns -> String -> GroupBy -> OrderBy -> PSQL [b]
selectAllRaw_ tn cols partSql g o = selectRaw_ tn cols partSql 0 0 g o

selectInRaw :: (ToField a, ToRow r, FromRow b) => TableName -> Columns -> Column -> [a] -> String -> r -> GroupBy -> OrderBy -> PSQL [b]
selectInRaw tn cols col xs partSql r = selectRaw tn cols ids ys from size
  where keyLen = length xs
        size = Size $ fromIntegral keyLen
        from = From 0
        vv = replicate keyLen "?"
        ids = unColumn col ++ " IN (" ++ columnsToString vv ++ ")" ++ getAnd partSql
        ys = toRow xs ++ toRow r

        getAnd :: String -> String
        getAnd ""  = ""
        getAnd sql = " AND " ++ sql

select :: (ToRow a, FromRow b) => TableName -> Columns -> String -> a -> From -> Size -> OrderBy -> PSQL [b]
select tn cols partSql a from size =
  selectRaw tn cols partSql a from size groupNone

select_ :: FromRow b => TableName -> Columns -> From -> Size -> OrderBy -> PSQL [b]
select_ tn cols from size = selectRaw_ tn cols "" from size groupNone

selectAll :: (ToRow a, FromRow b) => TableName -> Columns -> String -> a -> OrderBy -> PSQL [b]
selectAll tn cols partSql a = selectAllRaw tn cols partSql a groupNone

selectAll_ :: FromRow b => TableName -> Columns -> OrderBy -> PSQL [b]
selectAll_ tn cols = selectAllRaw_ tn cols "" groupNone

selectIn :: (ToField a, FromRow b) => TableName -> Columns -> Column -> [a] -> PSQL [b]
selectIn tn cols col xs = selectInRaw tn cols col xs "" () groupNone orderNone

selectOnly :: (ToRow a, FromRow (Only b)) => TableName -> Column -> String -> a -> From -> Size -> OrderBy -> PSQL [b]
selectOnly tn col partSql a from size o =
  map fromOnly <$> select tn [col] partSql a from size o

selectOnly_ :: FromRow (Only b) => TableName -> Column -> From -> Size -> OrderBy -> PSQL [b]
selectOnly_ tn col from size o =
  map fromOnly <$> select_ tn [col] from size o

selectAllOnly :: (ToRow a, FromRow (Only b)) => TableName -> Column -> String -> a -> OrderBy -> PSQL [b]
selectAllOnly tn col partSql a o =
  map fromOnly <$> selectAll tn [col] partSql a o

selectAllOnly_ :: FromRow (Only b) => TableName -> Column -> OrderBy -> PSQL [b]
selectAllOnly_ tn col o = map fromOnly <$> selectAll_ tn [col] o

selectOne :: (ToRow a, FromRow b) => TableName -> Columns -> String -> a -> PSQL (Maybe b)
selectOne tn cols partSql a = listToMaybe <$> select tn cols partSql a 0 1 orderNone

selectOneOnly :: (ToRow a, FromRow (Only b)) => TableName -> Column -> String -> a -> PSQL (Maybe b)
selectOneOnly tn col partSql a =
  fmap fromOnly <$> selectOne tn [col] partSql a

count :: ToRow a => TableName -> String -> a -> PSQL Int64
count tn partSql a = getOnlyDefault 0 <$> selectAll tn ["count(*)"] partSql a orderNone

count_ :: TableName -> PSQL Int64
count_ tn = getOnlyDefault 0 <$> selectAll_ tn ["count(*)"] orderNone
