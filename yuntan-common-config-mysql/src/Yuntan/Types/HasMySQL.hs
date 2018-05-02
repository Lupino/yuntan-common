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
  , otherEnv
  , SimpleLruEnv

  , VersionList
  , mergeDatabase

  , createConfigTable
  , initConfigState
  , setConfig_
  , getConfig_

  , setConfig
  , getConfig

  , setConfig'
  , getConfig'

  , cached'
  , cached
  , remove
  ) where


import           Control.Concurrent.Async
import           Control.Concurrent.QSem
import qualified Control.Exception        (SomeException, bracket_, try)
import           Control.Monad            (void)
import           Data.Aeson               (Value (..), decodeStrict, encode)
import           Data.ByteString          (ByteString)
import           Data.ByteString.Lazy     (toStrict)
import           Data.Hashable            (Hashable (..))
import           Data.Int                 (Int64)
import           Data.Maybe               (fromMaybe, listToMaybe)
import           Data.Pool                (Pool, withResource)
import           Data.String              (fromString)
import           Data.Typeable            (Typeable)
import           Database.MySQL.Simple    (Connection, Only (..), execute,
                                           execute_, query, query_)
import           Haxl.Core                hiding (fetchReq)
import           Haxl.Core.Monad          (unsafeLiftIO)

import           Data.HashPSQ             (delete, member)
import           Data.IORef               (atomicModifyIORef')
import           Data.LruCache
import           Data.LruCache.Internal   (LruCache (..))
import           Data.LruCache.IO         (LruHandle (..))
import           Prelude                  hiding (lookup)

type TablePrefix = String

type MySQL a = TablePrefix -> Connection -> IO a

class HasMySQL u where
  mysqlPool   :: u -> Pool Connection
  tablePrefix :: u -> TablePrefix

data SimpleEnv u = SimpleEnv { pc       :: Pool Connection
                             , pf       :: String
                             , otherEnv :: u
                             }

instance HasMySQL (SimpleEnv u) where
  mysqlPool = pc
  tablePrefix = pf

simpleEnv :: Pool Connection -> TablePrefix -> u -> SimpleEnv u
simpleEnv pool prefix env0 = SimpleEnv{pc=pool, pf = prefix, otherEnv = env0}

type SimpleLruEnv = SimpleEnv (Maybe (LruHandle String ByteString))

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
fetchAsync sem env req = async $
  Control.Exception.bracket_ (waitQSem sem) (signalQSem sem) $ withResource pool $ fetchSync req prefix

  where pool   = mysqlPool env
        prefix = tablePrefix env

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

getConfig' :: HasMySQL u => (u -> Maybe (LruHandle String ByteString)) -> String -> GenHaxl u (Maybe ByteString)
getConfig' lru k = cached lru k (getConfig k)

setConfig' :: HasMySQL u => (u -> Maybe (LruHandle String ByteString)) -> String -> ByteString -> GenHaxl u ()
setConfig' lru k v = do
  remove lru k
  setConfig k v

-- LruCache
doLookup :: (Hashable k, Ord k) => k -> LruCache k v -> (LruCache k v, Maybe v)
doLookup k c = case lookup k c of
                 Nothing      -> (c, Nothing)
                 Just (v, c') -> (c', Just v)

doInsert :: (Hashable k, Ord k) => k -> v -> a -> LruCache k v -> (LruCache k v, a)
doInsert k v v0 c = (insert k v c, v0)

-- | Return the cached result of the action or, in the case of a cache
-- miss, execute the action and insert it in the cache.
cached :: (Hashable k, Ord k) => (u -> Maybe (LruHandle k v)) -> k -> GenHaxl u (Maybe v) -> GenHaxl u (Maybe v)
cached lru k io = do
  h <- lru <$> env userEnv
  go h k io

  where go :: (Hashable k, Ord k) => Maybe (LruHandle k v) -> k -> GenHaxl u (Maybe v) -> GenHaxl u (Maybe v)
        go Nothing _ io0 = io0
        go (Just (LruHandle ref)) k0 io0 = do
          res <- unsafeLiftIO $ atomicModifyIORef' ref $ doLookup k0
          case res of
            Just v -> return (Just v)
            Nothing -> do
              v <- io0
              case v of
                Nothing -> return Nothing
                Just v0 -> unsafeLiftIO $ atomicModifyIORef' ref $ doInsert k0 v0 v

cached' :: (Hashable k, Ord k) => (u -> Maybe (LruHandle k v)) -> k -> GenHaxl u v -> GenHaxl u v
cached' lru k io = do
  h <- lru <$> env userEnv
  go h k io
  where go :: (Hashable k, Ord k) => Maybe (LruHandle k v) -> k -> GenHaxl u v -> GenHaxl u v
        go Nothing _ io0 = io0
        go (Just (LruHandle ref)) k0 io0 = do
          res <- unsafeLiftIO $ atomicModifyIORef' ref $ doLookup k0
          case res of
            Just v -> return v
            Nothing -> do
              v <- io0
              unsafeLiftIO $ atomicModifyIORef' ref $ doInsert k0 v v

remove :: (Hashable k, Ord k) => (u -> Maybe (LruHandle k v)) -> k -> GenHaxl u ()
remove lru k = do
  h <- lru <$> env userEnv
  case h of
    Nothing -> return ()
    Just (LruHandle ref) ->
      unsafeLiftIO $ atomicModifyIORef' ref $ \c -> do
        let queue = lruQueue c
            size  = lruSize c

        if member k queue then (c { lruSize = size - 1, lruQueue = delete k queue }, ())
                else (c, ())
