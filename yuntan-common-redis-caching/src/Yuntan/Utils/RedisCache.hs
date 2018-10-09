{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}

module Yuntan.Utils.RedisCache
  ( cached
  , cached'
  , remove
  , removeAll
  , initRedisState
  ) where

import           Control.Concurrent.Async
import           Control.Concurrent.QSem
import qualified Control.Exception        (SomeException, bracket_, try)
import           Control.Monad            (void)
import           Data.Aeson               (FromJSON, ToJSON, decodeStrict,
                                           encode)
import           Data.ByteString          (ByteString)
import           Data.ByteString.Lazy     (toStrict)
import           Data.Hashable            (Hashable (..))
import           Data.Typeable            (Typeable)
import           Database.Redis           (Connection, del, get, runRedis, set)
import           Haxl.Core                hiding (fetchReq)
import           Haxl.Core.Monad          (unsafeLiftIO)

newtype Conn = Conn Connection

instance Eq Conn where
  _ == _ = True

instance Show Conn where
  show _ = "Conn"

getData_ :: Connection -> ByteString -> IO (Maybe ByteString)
getData_ conn k = runRedis conn $ either (const Nothing) id <$> get k

setData :: ToJSON v => Connection -> ByteString -> v -> IO ()
setData conn k v = runRedis conn . void . set k . toStrict $ encode v

delData :: Connection -> [ByteString] -> IO ()
delData conn ks = runRedis conn . void $ del ks

-- Data source implementation.

data RedisReq a where
  GetData :: Conn -> ByteString -> RedisReq (Maybe ByteString)
  deriving (Typeable)

deriving instance Eq (RedisReq a)
instance Hashable (RedisReq a) where
  hashWithSalt s (GetData _ k) = hashWithSalt s (1::Int, k)

deriving instance Show (RedisReq a)
instance ShowP RedisReq where showp = show

instance StateKey RedisReq where
  data State RedisReq = RedisState { numThreads :: Int }

instance DataSourceName RedisReq where
  dataSourceName _ = "RedisDataSource"

instance DataSource u RedisReq where
  fetch = doFetch

doFetch
  :: State RedisReq
  -> Flags
  -> u
  -> PerformFetch RedisReq

doFetch _state _flags _user = AsyncFetch $ \reqs inner -> do
  sem <- newQSem $ numThreads _state
  asyncs <- mapM (fetchAsync sem _user) reqs
  inner
  mapM_ wait asyncs

fetchAsync :: QSem -> u -> BlockedFetch RedisReq -> IO (Async ())
fetchAsync sem _ req = async $
  Control.Exception.bracket_ (waitQSem sem) (signalQSem sem) $ fetchSync req

fetchSync :: BlockedFetch RedisReq -> IO ()
fetchSync (BlockedFetch req rvar) = do
  e <- Control.Exception.try $ fetchReq req
  case e of
    Left ex -> putFailure rvar (ex :: Control.Exception.SomeException)
    Right a -> putSuccess rvar a

fetchReq :: RedisReq a -> IO a
fetchReq (GetData (Conn conn) k) = getData_ conn k

initRedisState :: Int -> State RedisReq
initRedisState = RedisState

getData :: FromJSON v => Connection -> ByteString -> GenHaxl u (Maybe v)
getData conn k = maybe Nothing decodeStrict <$> dataFetch (GetData (Conn conn) k)

-- | Return the cached result of the action or, in the case of a cache
-- miss, execute the action and insert it in the cache.
cached :: (FromJSON v, ToJSON v) => (u -> Maybe Connection) -> ByteString -> GenHaxl u (Maybe v) -> GenHaxl u (Maybe v)
cached redis k io = do
  h <- redis <$> env userEnv
  go h k io

  where go :: (FromJSON v, ToJSON v) => Maybe Connection -> ByteString -> GenHaxl u (Maybe v) -> GenHaxl u (Maybe v)
        go Nothing _ io0 = io0
        go (Just conn) k0 io0 = do
          res <- getData conn k0
          case res of
            Just v -> return (Just v)
            Nothing -> do
              v <- io0
              case v of
                Nothing -> return Nothing
                Just v0 -> do
                  unsafeLiftIO $ setData conn k0 v0
                  return v

cached' :: (FromJSON v, ToJSON v) => (u -> Maybe Connection) -> ByteString -> GenHaxl u v -> GenHaxl u v
cached' redis k io = do
  h <- redis <$> env userEnv
  go h k io
  where go :: (FromJSON v, ToJSON v) => Maybe Connection -> ByteString -> GenHaxl u v -> GenHaxl u v
        go Nothing _ io0 = io0
        go (Just conn) k0 io0 = do
          res <- getData conn k0
          case res of
            Just v -> return v
            Nothing -> do
              v <- io0
              unsafeLiftIO $ setData conn k0 v
              return v

remove :: (u -> Maybe Connection) -> ByteString -> GenHaxl u ()
remove redis k = removeAll redis [k]

removeAll :: (u -> Maybe Connection) -> [ByteString] -> GenHaxl u ()
removeAll redis k = do
  h <- redis <$> env userEnv
  case h of
    Nothing   -> return ()
    Just conn -> unsafeLiftIO $ delData conn k
