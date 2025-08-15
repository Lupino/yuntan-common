{-# LANGUAGE OverloadedStrings #-}

module Database.PSQL.Gen
  ( constraintPrimaryKey
  , genSelect
  , genCreateTable
  , genCreateIndex
  , genInsert
  , genInsertOrUpdate
  , genUpdate
  , genDelete
  ) where

import           Data.String                     (IsString (..))
import           Database.PostgreSQL.Simple      (Query)
import           Database.PSQL.Types.Column      (Column (..), Columns,
                                                  columnsToString)
import           Database.PSQL.Types.GroupBy     (GroupBy)
import           Database.PSQL.Types.Page        (Page)
import           Database.PSQL.Types.TableName   (IndexName, TableName,
                                                  getIndexName, getTableName)
import           Database.PSQL.Types.TablePrefix (TablePrefix)

constraintPrimaryKey :: TablePrefix -> TableName -> Columns -> Column
constraintPrimaryKey prefix tn columns = Column . concat $
  [ "CONSTRAINT "
  , getIndexName prefix tn "pkey"
  , " PRIMARY KEY (", columnsToString columns, ")"
  ]

genWhere :: String -> String
genWhere "" = ""
genWhere ss = " WHERE " ++ ss

genSelect :: TableName -> Columns -> String -> Page -> GroupBy -> TablePrefix -> Query
genSelect tn cols partSql p g prefix =  fromString $ concat
  [ "SELECT ", columnsToString cols, " FROM ", getTableName prefix tn
  , genWhere partSql
  , show g
  , show p
  ]


genCreateTable :: TableName -> Columns -> TablePrefix -> Query
genCreateTable tn cols prefix = fromString $ concat
  [ "CREATE TABLE IF NOT EXISTS ", getTableName prefix tn, " ("
  , columnsToString cols
  , ")"
  ]

genCreateIndex :: Bool -> TableName -> IndexName -> Columns -> TablePrefix -> Query
genCreateIndex uniq tn idxN cols prefix = fromString $ concat
  [ "CREATE ", uniqWord, "INDEX IF NOT EXISTS ", getIndexName prefix tn idxN
  , " ON " , getTableName prefix tn, "(", columnsToString cols, ")"
  ]

  where uniqWord = if uniq then "UNIQUE " else ""

genInsertBase :: TableName -> Columns -> String -> TablePrefix -> Query
genInsertBase tn cols extSql prefix = fromString $ concat
  [ "INSERT INTO ", getTableName prefix tn
  , " (", columnsToString cols, ")"
  , " VALUES"
  , " (", columnsToString v, ")"
  , extSql
  ]
  where v = replicate (length cols) "?"


genInsert :: TableName -> Columns -> Maybe Column -> TablePrefix -> Query
genInsert tn cols mRetCol = genInsertBase tn cols (getRetCol mRetCol)
  where getRetCol Nothing    = ""
        getRetCol (Just col) = " RETURNING " ++ unColumn col

genInsertOrUpdate :: TableName -> Columns -> Columns -> Columns -> TablePrefix -> Query
genInsertOrUpdate tn uniqCols valCols otherCols = genInsertBase tn cols extSql
  where cols = uniqCols ++ valCols ++ otherCols

        genSetCol :: Column -> Column
        genSetCol (Column col) = Column $ col ++ " = excluded." ++ col

        genDoSql :: Columns -> String
        genDoSql [] = " DO NOTHING"
        genDoSql xs = " DO UPDATE SET " ++ columnsToString (map genSetCol xs)

        extSql = concat
          [ " ON CONFLICT (", columnsToString uniqCols, ")"
          , genDoSql valCols
          ]

genUpdate :: TableName -> Columns -> String -> TablePrefix -> Query
genUpdate tn cols partSql prefix = fromString $ concat
  [ "UPDATE ", getTableName prefix tn
  , " SET ", columnsToString setCols
  , genWhere partSql
  ]
  where setCols = map genSetCol cols

        genSetCol :: Column -> Column
        genSetCol (Column col)
          | '=' `elem` col = Column col
          | otherwise = Column $ col ++ " = ?"

genDelete :: TableName -> String -> TablePrefix -> Query
genDelete tn partSql prefix = fromString $ concat
  [ "DELETE FROM ", getTableName prefix tn, genWhere partSql
  ]
