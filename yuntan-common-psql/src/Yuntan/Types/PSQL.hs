{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module Yuntan.Types.PSQL
  ( TablePrefix

  , PSQLPool
  , PSQL
  , HasPSQL
  , psqlPool
  , tablePrefix
  , SimpleEnv
  , simpleEnv

  , HasOtherEnv
  , otherEnv

  , TableName
  , getTableName
  , Columns
  , createTable
  , constraintPrimaryKey
  , getIndexName
  , IndexName
  , createIndex

  , getOnly
  , getOnlyDefault

  , insert
  , insertRet
  , insertOrUpdate
  , update
  , delete
  , delete_
  , count
  , count_
  , select
  , selectOnly
  , select_
  , selectOnly_
  , selectOne
  , selectOneOnly

  , VersionList
  , mergeDatabase


  -- re-exports
  , FromRow (..)
  , field
  , Only (..)
  , SqlError (..)
  ) where


import           Control.Monad                      (void)
import           Data.Int                           (Int64)
import           Data.List                          (intercalate)
import           Data.Maybe                         (listToMaybe)
import           Data.Pool                          (Pool)
import           Data.String                        (IsString (..))
import           Database.PostgreSQL.Simple         (Connection, Only (..),
                                                     SqlError (..), ToRow,
                                                     execute, execute_, query,
                                                     query_)
import           Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import           Yuntan.Types.ListResult            (From, Size)
import           Yuntan.Types.OrderBy               (OrderBy, show1)


newtype TablePrefix = TablePrefix String
  deriving (Show)

instance IsString TablePrefix where
  fromString = TablePrefix

type PSQL a = TablePrefix -> Connection -> IO a
type PSQLPool = Pool Connection

class HasPSQL u where
  psqlPool    :: u -> PSQLPool
  tablePrefix :: u -> TablePrefix

class HasOtherEnv u a where
  otherEnv :: a -> u

data SimpleEnv u = SimpleEnv
    { pc :: Pool Connection
    , pf :: TablePrefix
    , pu :: u
    }

instance HasPSQL (SimpleEnv u) where
  psqlPool = pc
  tablePrefix = pf

instance HasOtherEnv u (SimpleEnv u) where
  otherEnv = pu

simpleEnv :: Pool Connection -> TablePrefix -> u -> SimpleEnv u
simpleEnv pool prefix env0 = SimpleEnv{pc=pool, pf = prefix, pu = env0}

newtype TableName = TableName String
  deriving (Show)

instance IsString TableName where
  fromString = TableName

getTableName :: TablePrefix -> TableName -> String
getTableName (TablePrefix "") (TableName name) =
  concat ["\"", name, "\"" ]
getTableName (TablePrefix prefix) (TableName name) =
  concat ["\"", prefix, "_", name, "\"" ]

newtype Column = Column { unColumn :: String }
  deriving (Show)

instance IsString Column where
  fromString = Column

type Columns = [Column]

columnsToString :: Columns -> String
columnsToString = intercalate ", " . map unColumn

constraintPrimaryKey :: TablePrefix -> TableName -> Columns -> Column
constraintPrimaryKey prefix tn columns = Column . concat $
  [ "CONSTRAINT "
  , getIndexName prefix tn "pkey"
  , " PRIMARY KEY (", columnsToString columns, ")"
  ]

createTable :: TableName -> Columns -> PSQL Int64
createTable tn cols prefix conn = execute_ conn sql
  where sql = fromString $ concat
          [ "CREATE TABLE IF NOT EXISTS ", getTableName prefix tn, " ("
          , columnsToString cols
          , ")"
          ]

newtype IndexName = IndexName String
  deriving (Show)

instance IsString IndexName where
  fromString = IndexName

getIndexName :: TablePrefix -> TableName -> IndexName -> String
getIndexName (TablePrefix "") (TableName tn) (IndexName name) =
  concat [ "\"", tn, "_", name, "\"" ]
getIndexName (TablePrefix prefix) (TableName tn) (IndexName name) =
  concat [ "\"", prefix, "_", tn , "_", name, "\"" ]


createIndex :: Bool -> TableName -> IndexName -> Columns -> PSQL Int64
createIndex uniq tn idxN cols prefix conn = execute_ conn sql
  where sql = fromString $ concat
          [ "CREATE ", uniqWord, "INDEX IF NOT EXISTS ", getIndexName prefix tn idxN
          , " ON " , getTableName prefix tn, "(", columnsToString cols, ")"
          ]

        uniqWord = if uniq then "UNIQUE " else ""

getOnly :: FromRow (Only a) => [Only a] -> Maybe a
getOnly = fmap fromOnly . listToMaybe

getOnlyDefault :: FromRow (Only a) => a -> [Only a] -> a
getOnlyDefault a = maybe a fromOnly . listToMaybe

insert :: ToRow a => TableName -> Columns -> a -> PSQL Int64
insert tn cols a prefix conn = execute conn sql a
  where v = take (length cols) $ cycle ["?"]
        sql = fromString $ concat
          [ "INSERT INTO ", getTableName prefix tn
          , " (", columnsToString cols, ")"
          , " VALUES"
          , " (", columnsToString v, ")"
          ]

insertRet :: (ToRow a, FromRow (Only b)) => TableName -> Columns -> Column -> a -> b -> PSQL b
insertRet tn cols col a def prefix conn = getOnlyDefault def <$> query conn sql a
  where v = take (length cols) $ cycle ["?"]
        sql = fromString $ concat
          [ "INSERT INTO ", getTableName prefix tn
          , " (", columnsToString cols, ")"
          , " VALUES"
          , " (", columnsToString v, ")"
          , " returning ", unColumn col
          ]

insertOrUpdate :: ToRow a => TableName -> Columns -> Columns -> Columns -> a -> PSQL Int64
insertOrUpdate tn uniqCols valCols otherCols a prefix conn = execute conn sql a
  where cols = uniqCols ++ valCols ++ otherCols
        v = replicate (length cols) "?"

        setSql = intercalate ", " $ map appendSet valCols

        appendSet :: Column -> String
        appendSet (Column col) | '=' `elem` col = col
                               | otherwise = col ++ " = excluded." ++ col

        doSql = if null valCols then " DO NOTHING" else " DO UPDATE SET " ++ setSql

        sql = fromString $ concat
          [ "INSERT INTO ", getTableName prefix tn
          , " (", columnsToString cols, ")"
          , " VALUES"
          , " (", columnsToString v, ")"
          , " ON CONFLICT (", columnsToString uniqCols, ")"
          , doSql
          ]

update :: ToRow a => TableName -> Columns -> String -> a -> PSQL Int64
update tn cols partSql a prefix conn = execute conn sql a
  where setSql = intercalate ", " $ map appendSet cols
        whereSql = if null partSql then "" else " WHERE " ++ partSql
        sql = fromString $ concat
          [ "UPDATE ", getTableName prefix tn
          , " SET ", setSql
          , whereSql
          ]

        appendSet :: Column -> String
        appendSet (Column col) | '=' `elem` col = col
                               | otherwise = col ++ " = ?"

delete :: ToRow a => TableName -> String -> a -> PSQL Int64
delete tn partSql a prefix conn = execute conn sql a
  where whereSql = " WHERE " ++ partSql
        sql = fromString $ concat
          [ "DELETE FROM ", getTableName prefix tn, whereSql
          ]

delete_ :: TableName -> PSQL Int64
delete_ tn prefix conn = execute_ conn sql
  where sql = fromString $ concat
          [ "DELETE FROM ", getTableName prefix tn
          ]

count :: ToRow a => TableName -> String -> a -> PSQL Int64
count tn partSql a prefix conn =
  getOnlyDefault 0 <$> query conn sql a
  where whereSql = " WHERE " ++ partSql
        sql = fromString $ concat
          [ "SELECT count(*) FROM ", getTableName prefix tn, whereSql
          ]

count_ :: TableName -> PSQL Int64
count_ tn prefix conn =
  getOnlyDefault 0 <$> query_ conn sql
  where sql = fromString $ concat
          [ "SELECT count(*) FROM ", getTableName prefix tn
          ]

select :: (ToRow a, FromRow b) => TableName -> Columns -> String -> a -> From -> Size -> OrderBy -> PSQL [b]
select tn cols partSql a from size o prefix conn = query conn sql a
  where whereSql = " WHERE " ++ partSql
        sql = fromString $ concat
          [ "SELECT ", columnsToString cols, " FROM ", getTableName prefix tn
          , whereSql
          , " ", show1 o
          , " LIMIT ", show size
          , " OFFSET ", show from
          ]

selectOnly :: (ToRow a, FromRow (Only b)) => TableName -> Column -> String -> a -> From -> Size -> OrderBy -> PSQL [b]
selectOnly tn col partSql a from size o prefix conn =
  map fromOnly <$> select tn [col] partSql a from size o prefix conn

select_ :: FromRow b => TableName -> Columns -> From -> Size -> OrderBy -> PSQL [b]
select_ tn cols from size o prefix conn = query_ conn sql
  where sql = fromString $ concat
          [ "SELECT ", columnsToString cols, " FROM ", getTableName prefix tn
          , " ", show1 o
          , " LIMIT ", show size
          , " OFFSET ", show from
          ]

selectOnly_ :: FromRow (Only b) => TableName -> Column -> From -> Size -> OrderBy -> PSQL [b]
selectOnly_ tn col from size o prefix conn =
  map fromOnly <$> select_ tn [col] from size o prefix conn

selectOne :: (ToRow a, FromRow b) => TableName -> Columns -> String -> a -> PSQL (Maybe b)
selectOne tn cols partSql a prefix conn = listToMaybe <$> query conn sql a
  where whereSql = " WHERE " ++ partSql
        sql = fromString $ concat
          [ "SELECT ", columnsToString cols, " FROM ", getTableName prefix tn
          , whereSql
          ]

selectOneOnly :: (ToRow a, FromRow (Only b)) => TableName -> Column -> String -> a -> PSQL (Maybe b)
selectOneOnly tn col partSql a prefix conn =
  fmap fromOnly <$> selectOne tn [col] partSql a prefix conn

createVersionTable :: PSQL Int64
createVersionTable prefix conn =
  createTable "version"
    [ "name VARCHAR(10) NOT NULL"
    , "version INT DEFAULT '0'"
    , "PRIMARY KEY (name)"
    ] prefix conn

getCurrentVersion :: PSQL Int64
getCurrentVersion prefix conn = do
  void $ createVersionTable prefix conn
  ts <- selectOneOnly "version" "version" "name = ?" (Only ("version" :: String)) prefix conn
  case ts of
    Just v -> pure v
    Nothing  ->
      insertRet "version" ["name", "version"] "version" ("version" :: String, 0 :: Int) 0 prefix conn


updateVersion :: Int64 -> PSQL ()
updateVersion ts prefix conn =
  void $ update "version" ["version"] "name = ?" (ts, "version" :: String) prefix conn

type Version a = (Int64, [PSQL a])
type VersionList a = [Version a]

mergeDatabase :: VersionList a -> PSQL ()
mergeDatabase versionList prefix conn = do
  version <- getCurrentVersion prefix conn
  mapM_ (\v -> processAction version v prefix conn) versionList

processAction :: Int64 -> Version a -> PSQL ()
processAction version (ts, actions) prefix conn =
  if ts > version then do
                  updateVersion ts prefix conn
                  mapM_ (\o -> void $ o prefix conn) actions
                  else pure ()
