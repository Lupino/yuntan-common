{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}

module Haxl.RedisCache
  ( cached
  , cached'
  , remove
  , removeAll
  , initRedisState
  , setRedisPrefix
  , set
  , get
  , genKey
  ) where

import           Control.Concurrent.Async
import           Control.Concurrent.QSem
import qualified Control.Exception        (SomeException, bracket_, try)
import           Control.Monad            (void)
import           Data.Aeson               (FromJSON, ToJSON, decodeStrict,
                                           encode)
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as B (concat)
import           Data.ByteString.Lazy     (toStrict)
import           Data.Hashable            (Hashable (..))
import           Data.IORef               (IORef, newIORef, readIORef,
                                           writeIORef)
import           Data.Typeable            (Typeable)
import           Database.Redis           (Connection, runRedis)
import qualified Database.Redis           as R (del, get, set)
import           Haxl.Core                hiding (fetchReq)

newtype Conn = Conn Connection

instance Eq Conn where
  _ == _ = True

instance Show Conn where
  show _ = "Conn"

genKey_ :: ByteString -> ByteString -> ByteString
genKey_ pref k = B.concat [pref, ":", k]

getData_ :: Connection -> ByteString -> IO (Maybe ByteString)
getData_ conn k = runRedis conn $ either (const Nothing) id <$> R.get k

setData_ :: Connection -> ByteString -> ByteString -> IO ()
setData_ conn k = runRedis conn . void . R.set k

delData_ :: Connection -> [ByteString] -> IO ()
delData_ conn ks = runRedis conn . void $ R.del ks

-- Data source implementation.

data RedisReq a where
  GetData :: Conn -> ByteString -> RedisReq (Maybe ByteString)
  SetData :: Conn -> ByteString -> ByteString -> RedisReq ()
  DelData :: Conn -> [ByteString] -> RedisReq ()
  GenKey  :: ByteString -> RedisReq ByteString
  deriving (Typeable)

deriving instance Eq (RedisReq a)
instance Hashable (RedisReq a) where
  hashWithSalt s (GetData _ k)   = hashWithSalt s (1::Int, k)
  hashWithSalt s (SetData _ k v) = hashWithSalt s (2::Int, k, v)
  hashWithSalt s (DelData _ ks)  = hashWithSalt s (3::Int, ks)
  hashWithSalt s (GenKey k)      = hashWithSalt s (4::Int, k)

deriving instance Show (RedisReq a)
instance ShowP RedisReq where showp = show

instance StateKey RedisReq where
  data State RedisReq = RedisState { numThreads :: Int, redisPrefix :: IORef ByteString }

instance DataSourceName RedisReq where
  dataSourceName _ = "RedisDataSource"

instance DataSource u RedisReq where
  fetch = doFetch

doFetch
  :: State RedisReq
  -> Flags
  -> u
  -> PerformFetch RedisReq

doFetch _state _flags _ = AsyncFetch $ \reqs inner -> do
  sem <- newQSem $ numThreads _state
  pref <- readIORef $ redisPrefix _state
  asyncs <- mapM (fetchAsync sem pref) reqs
  inner
  mapM_ wait asyncs

fetchAsync :: QSem -> ByteString -> BlockedFetch RedisReq -> IO (Async ())
fetchAsync sem pref req = async $
  Control.Exception.bracket_ (waitQSem sem) (signalQSem sem) $ fetchSync pref req

fetchSync :: ByteString -> BlockedFetch RedisReq -> IO ()
fetchSync pref (BlockedFetch req rvar) = do
  e <- Control.Exception.try $ fetchReq pref req
  case e of
    Left ex -> putFailure rvar (ex :: Control.Exception.SomeException)
    Right a -> putSuccess rvar a

fetchReq :: ByteString -> RedisReq a -> IO a
fetchReq pref (GetData (Conn conn) k)   = getData_ conn $ genKey_ pref k
fetchReq pref (SetData (Conn conn) k v) = setData_ conn (genKey_ pref k) v
fetchReq pref (DelData (Conn conn) ks)  = delData_ conn $ map (genKey_ pref) ks
fetchReq pref (GenKey k)                = return $ genKey_ pref k

initRedisState :: Int -> ByteString -> IO (State RedisReq)
initRedisState numThreads pref = do
  redisPrefix <- newIORef pref
  return $ RedisState {..}


setRedisPrefix :: State RedisReq -> ByteString -> IO ()
setRedisPrefix ds = writeIORef (redisPrefix ds)

getData :: FromJSON v => Connection -> ByteString -> GenHaxl u w (Maybe v)
getData conn k = maybe Nothing decodeStrict <$> dataFetch (GetData (Conn conn) k)

setData :: ToJSON v => Connection -> ByteString -> v -> GenHaxl u w ()
setData conn k v = uncachedRequest . SetData (Conn conn) k . toStrict $ encode v

delData :: Connection -> [ByteString] -> GenHaxl u w ()
delData conn = uncachedRequest . DelData (Conn conn)

-- | Return the cached result of the action or, in the case of a cache
-- miss, execute the action and insert it in the cache.
cached :: (FromJSON v, ToJSON v) => (u -> Maybe Connection) -> ByteString -> GenHaxl u w (Maybe v) -> GenHaxl u w (Maybe v)
cached redis k io = do
  h <- redis <$> env userEnv
  go h k io

  where go :: (FromJSON v, ToJSON v) => Maybe Connection -> ByteString -> GenHaxl u w (Maybe v) -> GenHaxl u w (Maybe v)
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
                  setData conn k0 v0
                  return v

cached' :: (FromJSON v, ToJSON v) => (u -> Maybe Connection) -> ByteString -> GenHaxl u w v -> GenHaxl u w v
cached' redis k io = do
  h <- redis <$> env userEnv
  go h k io
  where go :: (FromJSON v, ToJSON v) => Maybe Connection -> ByteString -> GenHaxl u w v -> GenHaxl u w v
        go Nothing _ io0 = io0
        go (Just conn) k0 io0 = do
          res <- getData conn k0
          case res of
            Just v -> return v
            Nothing -> do
              v <- io0
              setData conn k0 v
              return v

remove :: (u -> Maybe Connection) -> ByteString -> GenHaxl u w ()
remove redis k = removeAll redis [k]

removeAll :: (u -> Maybe Connection) -> [ByteString] -> GenHaxl u w ()
removeAll redis k = do
  h <- redis <$> env userEnv
  case h of
    Nothing   -> return ()
    Just conn -> delData conn k

get :: FromJSON v => (u -> Maybe Connection) -> ByteString -> GenHaxl u w (Maybe v)
get redis k = do
  h <- redis <$> env userEnv
  case h of
    Nothing   -> return Nothing
    Just conn -> getData conn k

set :: ToJSON v => (u -> Maybe Connection) -> ByteString -> v -> GenHaxl u w ()
set redis k v = do
  h <- redis <$> env userEnv
  case h of
    Nothing   -> return ()
    Just conn -> setData conn k v

genKey :: ByteString -> GenHaxl v w ByteString
genKey = dataFetch . GenKey
