{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Database.PSQL.Exc
  ( createTable
  , createIndex
  , insert
  , insertRet
  , insertOrUpdate
  , update
  , delete
  , delete_
  ) where


import           Data.Int                           (Int64)
import           Database.PostgreSQL.Simple         (Only (..))
import           Database.PostgreSQL.Simple.FromRow (FromRow (..))
import           Database.PostgreSQL.Simple.ToRow   (ToRow (..))
import           Database.PSQL.Gen                  (genCreateIndex,
                                                     genCreateTable, genDelete,
                                                     genInsert,
                                                     genInsertOrUpdate,
                                                     genUpdate)
import           Database.PSQL.Types.Column         (Column (..), Columns)
import           Database.PSQL.Types.PSQL           (PSQL, execute, execute_,
                                                     query)
import           Database.PSQL.Types.TableName      (IndexName, TableName)
import           Database.PSQL.Util                 (getOnlyDefault)

createTable :: TableName -> Columns -> PSQL Int64
createTable tn = execute_ . genCreateTable tn

createIndex :: Bool -> TableName -> IndexName -> Columns -> PSQL Int64
createIndex uniq tn idxN = execute_ . genCreateIndex uniq tn idxN

insert :: ToRow a => TableName -> Columns -> a -> PSQL Int64
insert tn cols = execute (genInsert tn cols Nothing)

insertRet :: (ToRow a, FromRow (Only b)) => TableName -> Columns -> Column -> a -> b -> PSQL b
insertRet tn cols col a def =
  getOnlyDefault def <$> query (genInsert tn cols (Just col)) a

insertOrUpdate :: ToRow a => TableName -> Columns -> Columns -> Columns -> a -> PSQL Int64
insertOrUpdate tn uniqCols valCols otherCols = execute (genInsertOrUpdate tn uniqCols valCols otherCols)

update :: ToRow a => TableName -> Columns -> String -> a -> PSQL Int64
update tn cols partSql = execute (genUpdate tn cols partSql)

delete :: ToRow a => TableName -> String -> a -> PSQL Int64
delete tn partSql = execute (genDelete tn partSql)

delete_ :: TableName -> PSQL Int64
delete_ tn = execute_ (genDelete tn "")
