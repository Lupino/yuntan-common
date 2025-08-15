{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Database.PSQL.Select
  ( count
  , count_
  , countInRaw

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
import           Data.Maybe                         (listToMaybe)
import           Database.PostgreSQL.Simple         (Only (..))
import           Database.PostgreSQL.Simple.FromRow (FromRow (..))
import           Database.PostgreSQL.Simple.ToField (ToField (..))
import           Database.PostgreSQL.Simple.ToRow   (ToRow (..))
import           Database.PSQL.Gen                  (genSelect)
import           Database.PSQL.Types.Column         (Column (..), Columns,
                                                     columnsToString)
import           Database.PSQL.Types.GroupBy        (GroupBy, groupNone)
import           Database.PSQL.Types.Page           (Page, pageNone, pageOne)
import           Database.PSQL.Types.PSQL           (PSQL, query, query_)
import           Database.PSQL.Types.TableName      (TableName)
import           Database.PSQL.Util                 (getOnlyDefault)

selectRaw :: (ToRow a, FromRow b) => TableName -> Columns -> String -> a -> Page -> GroupBy -> PSQL [b]
selectRaw tn cols partSql a p g = query sql a
  where sql = genSelect tn cols partSql p g

selectRaw_ :: FromRow b => TableName -> Columns -> String -> Page -> GroupBy -> PSQL [b]
selectRaw_ tn cols partSql p g = query_ sql
  where sql = genSelect tn cols partSql p g

selectAllRaw :: (ToRow a, FromRow b) => TableName -> Columns -> String -> a -> GroupBy -> PSQL [b]
selectAllRaw tn cols partSql a = selectRaw tn cols partSql a pageNone

selectAllRaw_ :: FromRow b => TableName -> Columns -> String -> GroupBy -> PSQL [b]
selectAllRaw_ tn cols partSql = selectRaw_ tn cols partSql pageNone

selectInRaw :: (ToField a, ToRow r, FromRow b) => TableName -> Columns -> Column -> [a] -> String -> r -> Page -> GroupBy -> PSQL [b]
selectInRaw tn cols col xs partSql r = selectRaw tn cols ids ys
  where keyLen = length xs
        vv = replicate keyLen "?"
        ids = unColumn col ++ " IN (" ++ columnsToString vv ++ ")" ++ getAnd partSql
        ys = toRow xs ++ toRow r

        getAnd :: String -> String
        getAnd ""  = ""
        getAnd sql = " AND " ++ sql

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

selectInOnly :: (ToField a, ToRow r, FromRow (Only b)) => TableName -> Columns -> Column -> [a] -> String -> r -> Page -> PSQL [b]
selectInOnly tn cols col xs partSql a p =
  map fromOnly <$> selectInRaw tn cols col xs partSql a p groupNone

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

count :: ToRow a => TableName -> String -> a -> PSQL Int64
count tn partSql a = getOnlyDefault 0 <$> selectAll tn ["count(*)"] partSql a

count_ :: TableName -> PSQL Int64
count_ tn = getOnlyDefault 0 <$> selectAll_ tn ["count(*)"]

countInRaw :: (ToField a, ToRow r) => TableName -> Column -> [a] -> String -> r -> PSQL Int64
countInRaw tn col xs partSql a =
  getOnlyDefault 0 <$> selectInRaw tn ["count(*)"] col xs partSql a pageOne groupNone
