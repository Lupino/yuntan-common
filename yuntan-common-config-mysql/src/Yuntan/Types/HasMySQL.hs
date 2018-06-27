{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}

module Yuntan.Types.HasMySQL
  (
    TablePrefix
  , MySQL
  , HasMySQL
  , mysqlPool
  , tablePrefix
  , SimpleEnv
  , simpleEnv

  , HasOtherEnv
  , otherEnv

  , VersionList
  , mergeDatabase

  , createConfigTable
  , initConfigState
  , setConfig_
  , getConfig_

  , setConfig
  , getConfig
  , getConfigJSON

  , setConfig'
  , getConfig'
  , getConfigJSON'
  , ConfigLru
  , fillValue
  , fillValue_
  ) where


import           Control.Concurrent.Async
import           Control.Concurrent.QSem
import qualified Control.Exception        (SomeException, bracket_, try)
import           Control.Monad            (void)
import           Data.Aeson               (FromJSON, Value, decodeStrict)
import           Data.ByteString          (ByteString)
import           Data.Hashable            (Hashable (..))
import           Data.Int                 (Int64)
import           Data.Maybe               (listToMaybe)
import           Data.Pool                (Pool, withResource)
import           Data.String              (fromString)
import           Data.Typeable            (Typeable)
import           Database.MySQL.Simple    (Connection, Only (..), execute,
                                           execute_, query, query_)
import           Haxl.Core                hiding (fetchReq)

import           Yuntan.Utils.JSON        (unionValue)
import           Yuntan.Utils.LruCache    (LruHandle, cached, remove)

type TablePrefix = String

type MySQL a = TablePrefix -> Connection -> IO a

class HasMySQL u where
  mysqlPool   :: u -> Pool Connection
  tablePrefix :: u -> TablePrefix

class HasOtherEnv u a where
  otherEnv :: a -> u

data SimpleEnv u = SimpleEnv { pc :: Pool Connection
                             , pf :: String
                             , pu :: u
                             }

instance HasMySQL (SimpleEnv u) where
  mysqlPool = pc
  tablePrefix = pf

instance HasOtherEnv u (SimpleEnv u) where
  otherEnv = pu

simpleEnv :: Pool Connection -> TablePrefix -> u -> SimpleEnv u
simpleEnv pool prefix env0 = SimpleEnv{pc=pool, pf = prefix, pu = env0}

createVersionTable :: MySQL ()
createVersionTable prefix conn = void $ execute_ conn sql
  where sql = fromString $ concat [ "CREATE TABLE IF NOT EXISTS `", prefix, "_version` ("
                                  , "  `name` varchar(128) NOT NULL,"
                                  , "  `version` int(10) unsigned DEFAULT '0',"
                                  , "  PRIMARY KEY (`name`)"
                                  , ") ENGINE=InnoDB DEFAULT CHARSET=utf8"
                                  ]

getCurrentVersion :: MySQL Int64
getCurrentVersion prefix conn = do
  void $ createVersionTable prefix conn
  ts <- query_ conn $ fromString $ concat
    [ "SELECT `version` FROM `", prefix, "_version`"
    , " WHERE `name` = 'version'"
    ]
  case listToMaybe ts of
    Just ts' -> pure (fromOnly ts')
    Nothing  -> do
      void $ execute_ conn $ fromString $ concat
        [ "INSERT INTO `", prefix, "_version`"
        , " (`name`, `version`)"
        , " VALUES"
        , " ('version', 0)"
        ]
      pure 0


updateVersion :: Int64 -> MySQL ()
updateVersion ts prefix conn =
  void $ execute_ conn $ fromString $ concat
    [ "UPDATE `", prefix, "_version`"
    , " SET `version` = ", show ts
    , " WHERE `name` = 'version'"
    ]

type Version = (Int64, [MySQL ()])
type VersionList = [Version]

mergeDatabase :: VersionList -> MySQL ()
mergeDatabase versionList prefix conn = do
  version <- getCurrentVersion prefix conn
  mapM_ (\v -> processAction version v prefix conn) versionList

processAction :: Int64 -> Version -> MySQL ()
processAction version (ts, actions) prefix conn =
  if ts > version then do
                  updateVersion ts prefix conn
                  mapM_ (\o -> o prefix conn) actions
                  else pure ()

-- config source

createConfigTable :: MySQL ()
createConfigTable prefix conn = void $ execute_ conn sql
  where sql = fromString $ concat [ "CREATE TABLE IF NOT EXISTS `", prefix, "_config` ("
                                  , "  `key` varchar(128) NOT NULL,"
                                  , "  `value` TEXT DEFAULT NULL,"
                                  , "  PRIMARY KEY (`key`)"
                                  , ") ENGINE=InnoDB DEFAULT CHARSET=utf8"
                                  ]

setConfig_ :: String -> ByteString -> MySQL ()
setConfig_ key value prefix conn = void $ execute conn sql (key, value)
  where sql = fromString $ concat ["REPLACE INTO `", prefix, "_config` (`key`, `value`) VALUES (?, ?)"]

getConfig_ :: String -> MySQL (Maybe ByteString)
getConfig_ key prefix conn = fmap fromOnly . listToMaybe <$> query conn sql (Only key)
  where sql = fromString $ concat ["SELECT `value` FROM `", prefix, "_config` WHERE `key` = ?"]


-- Data source implementation.

data ConfigReq a where
  SetConfig :: String -> ByteString -> ConfigReq ()
  GetConfig :: String -> ConfigReq (Maybe ByteString)
  deriving (Typeable)

deriving instance Eq (ConfigReq a)
instance Hashable (ConfigReq a) where
  hashWithSalt s (SetConfig k v) = hashWithSalt s (1::Int, k, v)
  hashWithSalt s (GetConfig k)   = hashWithSalt s (2::Int, k)

deriving instance Show (ConfigReq a)
instance ShowP ConfigReq where showp = show

instance StateKey ConfigReq where
  data State ConfigReq = ConfigState { numThreads :: Int }

instance DataSourceName ConfigReq where
  dataSourceName _ = "ConfigDataSource"

instance HasMySQL u => DataSource u ConfigReq where
  fetch = doFetch

doFetch
  :: HasMySQL u
  => State ConfigReq
  -> Flags
  -> u
  -> PerformFetch ConfigReq

doFetch _state _flags _user = AsyncFetch $ \reqs inner -> do
  sem <- newQSem $ numThreads _state
  asyncs <- mapM (fetchAsync sem _user) reqs
  inner
  mapM_ wait asyncs

fetchAsync :: HasMySQL u => QSem -> u -> BlockedFetch ConfigReq -> IO (Async ())
fetchAsync sem env0 req = async $
  Control.Exception.bracket_ (waitQSem sem) (signalQSem sem) $ withResource pool $ fetchSync req prefix

  where pool   = mysqlPool env0
        prefix = tablePrefix env0

fetchSync :: BlockedFetch ConfigReq -> MySQL ()
fetchSync (BlockedFetch req rvar) prefix conn = do
  e <- Control.Exception.try $ fetchReq req prefix conn
  case e of
    Left ex -> putFailure rvar (ex :: Control.Exception.SomeException)
    Right a -> putSuccess rvar a

fetchReq :: ConfigReq a -> MySQL a
fetchReq (GetConfig k)   = getConfig_ k
fetchReq (SetConfig k v) = setConfig_ k v

initConfigState :: Int -> State ConfigReq
initConfigState = ConfigState

getConfig :: HasMySQL u => String -> GenHaxl u (Maybe ByteString)
setConfig :: HasMySQL u => String -> ByteString -> GenHaxl u ()
getConfig   = dataFetch . GetConfig
setConfig k = uncachedRequest . SetConfig k

getConfigJSON :: (HasMySQL u, FromJSON a) => String -> GenHaxl u (Maybe a)
getConfigJSON k = maybe Nothing decodeStrict <$> getConfig k

type ConfigLru = Maybe (LruHandle String ByteString)

getConfig' :: HasMySQL u => (u -> ConfigLru) -> String -> GenHaxl u (Maybe ByteString)
getConfig' lru k = cached lru k (getConfig k)

getConfigJSON' :: (HasMySQL u, FromJSON a) => (u -> ConfigLru) -> String -> GenHaxl u (Maybe a)
getConfigJSON' lru k = maybe Nothing decodeStrict <$> cached lru k (getConfig k)

setConfig' :: HasMySQL u => (u -> ConfigLru) -> String -> ByteString -> GenHaxl u ()
setConfig' lru k v = do
  remove lru k
  setConfig k v

fillValue
  :: HasMySQL u
  => (u -> ConfigLru)
  -> String
  -> (a -> Value)
  -> (Value -> a -> a)
  -> Maybe a
  -> GenHaxl u (Maybe a)
fillValue _ _ _ _ Nothing    = return Nothing
fillValue lru k g f (Just v) = Just <$> fillValue_ lru k g f v

fillValue_
  :: HasMySQL u
  => (u -> ConfigLru)
  -> String
  -> (a -> Value)
  -> (Value -> a -> a)
  -> a
  -> GenHaxl u a
fillValue_ lru k g f v = do
  ex <- getConfigJSON' lru k
  case ex of
    Nothing  -> return v
    Just ex' -> return $ f (unionValue (g v) ex') v
