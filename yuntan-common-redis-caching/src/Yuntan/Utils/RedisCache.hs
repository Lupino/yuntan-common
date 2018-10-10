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
import qualified Data.ByteString          as B (concat)
import           Data.ByteString.Lazy     (toStrict)
import           Data.Hashable            (Hashable (..))
import           Data.Typeable            (Typeable)
import           Database.Redis           (Connection, del, get, runRedis, set)
import           Haxl.Core                hiding (fetchReq)

newtype Conn = Conn Connection

instance Eq Conn where
  _ == _ = True

instance Show Conn where
  show _ = "Conn"

genKey :: ByteString -> ByteString -> ByteString
genKey pref k = B.concat [pref, ":", k]

getData_ :: Connection -> ByteString -> IO (Maybe ByteString)
getData_ conn k = runRedis conn $ either (const Nothing) id <$> get k

setData_ :: Connection -> ByteString -> ByteString -> IO ()
setData_ conn k = runRedis conn . void . set k

delData_ :: Connection -> [ByteString] -> IO ()
delData_ conn ks = runRedis conn . void $ del ks

-- Data source implementation.

data RedisReq a where
  GetData :: Conn -> ByteString -> RedisReq (Maybe ByteString)
  SetData :: Conn -> ByteString -> ByteString -> RedisReq ()
  DelData :: Conn -> [ByteString] -> RedisReq ()
  deriving (Typeable)

deriving instance Eq (RedisReq a)
instance Hashable (RedisReq a) where
  hashWithSalt s (GetData _ k)   = hashWithSalt s (1::Int, k)
  hashWithSalt s (SetData _ k v) = hashWithSalt s (2::Int, k, v)
  hashWithSalt s (DelData _ ks)  = hashWithSalt s (3::Int, ks)

deriving instance Show (RedisReq a)
instance ShowP RedisReq where showp = show

instance StateKey RedisReq where
  data State RedisReq = RedisState { numThreads :: Int, prefix :: ByteString }

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
  asyncs <- mapM (fetchAsync sem (prefix _state)) reqs
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
fetchReq pref (GetData (Conn conn) k)   = getData_ conn $ genKey pref k
fetchReq pref (SetData (Conn conn) k v) = setData_ conn (genKey pref k) v
fetchReq pref (DelData (Conn conn) ks)  = delData_ conn $ map (genKey pref) ks

initRedisState :: Int -> ByteString -> State RedisReq
initRedisState = RedisState

getData :: FromJSON v => Connection -> ByteString -> GenHaxl u (Maybe v)
getData conn k = maybe Nothing decodeStrict <$> dataFetch (GetData (Conn conn) k)

setData :: ToJSON v => Connection -> ByteString -> v -> GenHaxl u ()
setData conn k v = uncachedRequest . SetData (Conn conn) k . toStrict $ encode v

delData :: Connection -> [ByteString] -> GenHaxl u ()
delData conn = uncachedRequest . DelData (Conn conn)

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
                  setData conn k0 v0
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
              setData conn k0 v
              return v

remove :: (u -> Maybe Connection) -> ByteString -> GenHaxl u ()
remove redis k = removeAll redis [k]

removeAll :: (u -> Maybe Connection) -> [ByteString] -> GenHaxl u ()
removeAll redis k = do
  h <- redis <$> env userEnv
  case h of
    Nothing   -> return ()
    Just conn -> delData conn k
