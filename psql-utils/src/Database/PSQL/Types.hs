{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

module Database.PSQL.Types
  ( TablePrefix

  , Connection
  , PSQLPool
  , createPSQLPool
  , PSQL
  , runPSQL
  , runPSQLPool
  , runPSQLEnv
  , getTablePrefix
  , HasPSQL
  , psqlPool
  , tablePrefix
  , SimpleEnv
  , simpleEnv

  , HasOtherEnv
  , otherEnv

  , TableName
  , as
  , join
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
  , selectIn

  , VersionList
  , mergeDatabase
  , withTransaction

  -- re-exports
  , FromRow (..)
  , FromField (..)
  , ToRow (..)
  , ToField (..)
  , field
  , Only (..)
  , SqlError (..)

  , From (..)
  , Size (..)
  , OrderBy
  , asc
  , desc
  , none
  ) where


import           Control.Monad                        (void, when)
import           Control.Monad.IO.Class               (MonadIO (..))
import           Data.ByteString                      (ByteString)
import           Data.Hashable                        (Hashable (..))
import           Data.Int                             (Int64)
import           Data.List                            (intercalate)
import           Data.Maybe                           (listToMaybe)
import           Data.Pool                            (Pool, defaultPoolConfig,
                                                       newPool, setNumStripes,
                                                       withResource)
import           Data.String                          (IsString (..))
import           Database.PostgreSQL.Simple           (Connection, Only (..),
                                                       SqlError (..), close,
                                                       connectPostgreSQL,
                                                       execute, execute_, query,
                                                       query_)
import qualified Database.PostgreSQL.Simple           as L (withTransaction)
import           Database.PostgreSQL.Simple.FromField (FromField (..))
import           Database.PostgreSQL.Simple.FromRow   (FromRow (..), field)
import           Database.PostgreSQL.Simple.ToField   (ToField (..))
import           Database.PostgreSQL.Simple.ToRow     (ToRow (..))
import           GHC.Generics                         (Generic)

newtype From = From { unFrom :: Int64 }
  deriving (Generic, Eq, Num, Integral, Real, Ord, Enum)
instance Show From where
  show = show . unFrom

instance Hashable From

newtype Size = Size {unSize :: Int64}
  deriving (Generic, Eq, Num, Integral, Real, Ord, Enum)

instance Show Size where
  show = show . unSize

instance Hashable Size

newtype TablePrefix = TablePrefix String
  deriving (Show)

instance IsString TablePrefix where
  fromString = TablePrefix

getPrefix :: TablePrefix -> String
getPrefix (TablePrefix "") = ""
getPrefix (TablePrefix x)  = x ++ "_"

type PSQLPool = Pool Connection

createPSQLPool :: ByteString -> Maybe Int -> Double -> Int -> IO PSQLPool
createPSQLPool path numStripes idleTime =
  newPool . setNumStripes numStripes . defaultPoolConfig (connectPostgreSQL path) close idleTime

newtype PSQL a = PSQL {unPSQL :: TablePrefix -> Connection -> IO a}

instance Functor PSQL where
  fmap f a = PSQL $ \p c -> f <$> unPSQL a p c
  {-# INLINE fmap #-}

instance Applicative PSQL where
  pure a = PSQL $ \_ _ -> pure a
  {-# INLINE pure #-}
  f <*> v = PSQL $ \p c -> unPSQL f p c <*> unPSQL v p c
  {-# INLINE (<*>) #-}


instance Monad PSQL where
  m >>= k = PSQL $ \p c -> do
    a <- unPSQL m p c
    unPSQL (k a) p c
  {-# INLINE (>>=) #-}

instance MonadIO PSQL where
  liftIO m = PSQL $ \_ _ -> m
  {-# INLINE liftIO #-}

runPSQL :: TablePrefix -> Connection -> PSQL a -> IO a
runPSQL prefix conn m = unPSQL m prefix conn

runPSQLPool :: TablePrefix -> PSQLPool -> PSQL a -> IO a
runPSQLPool prefix pool m = withResource pool $ unPSQL m prefix

getTablePrefix :: PSQL TablePrefix
getTablePrefix = PSQL $ \p _ -> return p

class HasPSQL u where
  psqlPool    :: u -> PSQLPool
  tablePrefix :: u -> TablePrefix

runPSQLEnv :: (HasPSQL env) => env -> PSQL a -> IO a
runPSQLEnv env = runPSQLPool (tablePrefix env) (psqlPool env)


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

data TableName =
  TableName String
  | TableNameAs TableName String
  | TableNameJoin TableName TableName
  deriving (Show)

instance IsString TableName where
  fromString = TableName

as :: TableName -> String -> TableName
tn `as` asName = TableNameAs tn asName

join :: TableName -> TableName -> TableName
name1 `join` name2 = TableNameJoin name1 name2

getTableName :: TablePrefix -> TableName -> String
getTableName prefix (TableName name) =
  concat ["\"", getPrefix prefix, name, "\"" ]
getTableName prefix (TableNameAs name asName) =
  concat [ getTableName prefix name, " AS ", asName ]
getTableName prefix (TableNameJoin name1 name2) =
  concat [ getTableName prefix name1, ", ", getTableName prefix name2 ]

newtype Column = Column { unColumn :: String }
  deriving (Show)

instance IsString Column where
  fromString = Column

type Columns = [Column]

columnsToString :: Columns -> String
columnsToString = intercalate ", " . map unColumn

columnsToString' :: Columns -> String
columnsToString' = intercalate ", " . map (takeWhile (/='=') . unColumn)

constraintPrimaryKey :: TablePrefix -> TableName -> Columns -> Column
constraintPrimaryKey prefix tn columns = Column . concat $
  [ "CONSTRAINT "
  , getIndexName prefix tn "pkey"
  , " PRIMARY KEY (", columnsToString columns, ")"
  ]

createTable :: TableName -> Columns -> PSQL Int64
createTable tn cols = PSQL $ \prefix conn -> execute_ conn (sql prefix)
  where sql prefix = fromString $ concat
          [ "CREATE TABLE IF NOT EXISTS ", getTableName prefix tn, " ("
          , columnsToString cols
          , ")"
          ]

newtype IndexName = IndexName String
  deriving (Show)

instance IsString IndexName where
  fromString = IndexName

getIndexName :: TablePrefix -> TableName -> IndexName -> String
getIndexName prefix (TableName tn) (IndexName name) =
  concat [ "\"", getPrefix prefix, tn , "_", name, "\"" ]
getIndexName _ _ _ = error "no implement"


createIndex :: Bool -> TableName -> IndexName -> Columns -> PSQL Int64
createIndex uniq tn idxN cols = PSQL $ \prefix conn -> execute_ conn (sql prefix)
  where sql prefix = fromString $ concat
          [ "CREATE ", uniqWord, "INDEX IF NOT EXISTS ", getIndexName prefix tn idxN
          , " ON " , getTableName prefix tn, "(", columnsToString cols, ")"
          ]

        uniqWord = if uniq then "UNIQUE " else ""

getOnly :: FromRow (Only a) => [Only a] -> Maybe a
getOnly = fmap fromOnly . listToMaybe

getOnlyDefault :: FromRow (Only a) => a -> [Only a] -> a
getOnlyDefault a = maybe a fromOnly . listToMaybe

insert :: ToRow a => TableName -> Columns -> a -> PSQL Int64
insert tn cols a = PSQL $ \prefix conn -> execute conn (sql prefix) a
  where v = replicate (length cols) "?"
        sql prefix = fromString $ concat
          [ "INSERT INTO ", getTableName prefix tn
          , " (", columnsToString cols, ")"
          , " VALUES"
          , " (", columnsToString v, ")"
          ]

insertRet :: (ToRow a, FromRow (Only b)) => TableName -> Columns -> Column -> a -> b -> PSQL b
insertRet tn cols col a def = PSQL $ \prefix conn -> getOnlyDefault def <$> query conn (sql prefix) a
  where v = replicate (length cols) "?"
        sql prefix = fromString $ concat
          [ "INSERT INTO ", getTableName prefix tn
          , " (", columnsToString cols, ")"
          , " VALUES"
          , " (", columnsToString v, ")"
          , " returning ", unColumn col
          ]

insertOrUpdate :: ToRow a => TableName -> Columns -> Columns -> Columns -> a -> PSQL Int64
insertOrUpdate tn uniqCols valCols otherCols a = PSQL $ \prefix conn -> execute conn (sql prefix) a
  where cols = uniqCols ++ valCols ++ otherCols
        v = replicate (length cols) "?"

        setSql = intercalate ", " $ map appendSet valCols

        appendSet :: Column -> String
        appendSet (Column col) | '=' `elem` col = col
                               | otherwise = col ++ " = excluded." ++ col

        doSql = if null valCols then " DO NOTHING" else " DO UPDATE SET " ++ setSql

        sql prefix = fromString $ concat
          [ "INSERT INTO ", getTableName prefix tn
          , " (", columnsToString' cols, ")"
          , " VALUES"
          , " (", columnsToString v, ")"
          , " ON CONFLICT (", columnsToString uniqCols, ")"
          , doSql
          ]

update :: ToRow a => TableName -> Columns -> String -> a -> PSQL Int64
update tn cols partSql a = PSQL $ \prefix conn -> execute conn (sql prefix) a
  where setSql = intercalate ", " $ map appendSet cols
        whereSql = if null partSql then "" else " WHERE " ++ partSql
        sql prefix = fromString $ concat
          [ "UPDATE ", getTableName prefix tn
          , " SET ", setSql
          , whereSql
          ]

        appendSet :: Column -> String
        appendSet (Column col) | '=' `elem` col = col
                               | otherwise = col ++ " = ?"

delete :: ToRow a => TableName -> String -> a -> PSQL Int64
delete tn partSql a = PSQL $ \prefix conn -> execute conn (sql prefix) a
  where whereSql = " WHERE " ++ partSql
        sql prefix = fromString $ concat
          [ "DELETE FROM ", getTableName prefix tn, whereSql
          ]

delete_ :: TableName -> PSQL Int64
delete_ tn = PSQL $ \prefix conn -> execute_ conn (sql prefix)
  where sql prefix = fromString $ "DELETE FROM " ++ getTableName prefix tn


count :: ToRow a => TableName -> String -> a -> PSQL Int64
count tn partSql a = PSQL $ \prefix conn ->
  getOnlyDefault 0 <$> query conn (sql prefix) a
  where whereSql = " WHERE " ++ partSql
        sql prefix = fromString $ concat
          [ "SELECT count(*) FROM ", getTableName prefix tn, whereSql
          ]

count_ :: TableName -> PSQL Int64
count_ tn = PSQL $ \prefix conn ->
  getOnlyDefault 0 <$> query_ conn (sql prefix)
  where sql prefix = fromString $ "SELECT count(*) FROM " ++ getTableName prefix tn


select :: (ToRow a, FromRow b) => TableName -> Columns -> String -> a -> From -> Size -> OrderBy -> PSQL [b]
select tn cols partSql a from size o = PSQL $ \prefix conn ->  query conn (sql prefix) a
  where whereSql = " WHERE " ++ partSql
        sql prefix = fromString $ concat
          [ "SELECT ", columnsToString cols, " FROM ", getTableName prefix tn
          , whereSql
          , " ", show o
          , " LIMIT ", show size
          , " OFFSET ", show from
          ]

selectIn :: (ToField a, FromRow b) => TableName -> Columns -> Column -> [a] -> PSQL [b]
selectIn tn cols col xs = select tn cols ids xs from size none
  where keyLen = length xs
        size = Size $ fromIntegral keyLen
        from = From 0
        vv = replicate keyLen "?"
        ids = unColumn col ++ " IN (" ++ columnsToString vv ++ ")"

selectOnly :: (ToRow a, FromRow (Only b)) => TableName -> Column -> String -> a -> From -> Size -> OrderBy -> PSQL [b]
selectOnly tn col partSql a from size o =
  map fromOnly <$> select tn [col] partSql a from size o

select_ :: FromRow b => TableName -> Columns -> From -> Size -> OrderBy -> PSQL [b]
select_ tn cols from size o = PSQL $ \prefix conn -> query_ conn (sql prefix)
  where sql prefix = fromString $ concat
          [ "SELECT ", columnsToString cols, " FROM ", getTableName prefix tn
          , " ", show o
          , " LIMIT ", show size
          , " OFFSET ", show from
          ]

selectOnly_ :: FromRow (Only b) => TableName -> Column -> From -> Size -> OrderBy -> PSQL [b]
selectOnly_ tn col from size o =
  map fromOnly <$> select_ tn [col] from size o

selectOne :: (ToRow a, FromRow b) => TableName -> Columns -> String -> a -> PSQL (Maybe b)
selectOne tn cols partSql a = PSQL $ \prefix conn -> listToMaybe <$> query conn (sql prefix) a
  where whereSql = " WHERE " ++ partSql
        sql prefix = fromString $ concat
          [ "SELECT ", columnsToString cols, " FROM ", getTableName prefix tn
          , whereSql
          ]

selectOneOnly :: (ToRow a, FromRow (Only b)) => TableName -> Column -> String -> a -> PSQL (Maybe b)
selectOneOnly tn col partSql a =
  fmap fromOnly <$> selectOne tn [col] partSql a

createVersionTable :: PSQL Int64
createVersionTable =
  createTable "version"
    [ "name VARCHAR(10) NOT NULL"
    , "version INT DEFAULT '0'"
    , "PRIMARY KEY (name)"
    ]

getCurrentVersion :: PSQL Int64
getCurrentVersion = do
  void createVersionTable
  ts <- selectOneOnly "version" "version" "name = ?" (Only ("version" :: String))
  case ts of
    Just v -> pure v
    Nothing  ->
      insertRet "version" ["name", "version"] "version" ("version" :: String, 0 :: Int) 0


updateVersion :: Int64 -> PSQL ()
updateVersion ts =
  void $ update "version" ["version"] "name = ?" (ts, "version" :: String)

type Version a = (Int64, [PSQL a])
type VersionList a = [Version a]

mergeDatabase :: VersionList a -> PSQL ()
mergeDatabase versionList = withTransaction $ do
  version <- getCurrentVersion
  mapM_ (processAction version) versionList

processAction :: Int64 -> Version a -> PSQL ()
processAction version (ts, actions) =
  when (ts > version) $ do
    updateVersion ts
    mapM_ void actions

data OrderBy = Desc String | Asc String | None
  deriving (Generic, Eq)

instance Hashable OrderBy

desc :: String -> OrderBy
desc = Desc

asc :: String -> OrderBy
asc = Asc

none :: OrderBy
none = None

instance Show OrderBy where
  show (Desc f) = "ORDER BY " ++ f ++ " DESC"
  show (Asc f)  = "ORDER BY " ++ f ++ " ASC"
  show None     = ""

withTransaction :: PSQL a -> PSQL a
withTransaction m = PSQL $ \prefix conn ->
  L.withTransaction conn $ runPSQL prefix conn m
