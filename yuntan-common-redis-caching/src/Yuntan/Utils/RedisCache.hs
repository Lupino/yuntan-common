module Yuntan.Utils.RedisCache
  ( cached
  , cached'
  , remove
  , removeAll
  ) where


import           Control.Monad        (void)
import           Data.Aeson           (FromJSON, ToJSON, decodeStrict, encode)
import           Data.ByteString      (ByteString)
import           Data.ByteString.Lazy (toStrict)
import           Database.Redis       (Connection, del, get, runRedis, set)
import           Haxl.Core            (GenHaxl, env, userEnv)
import           Haxl.Core.Monad      (unsafeLiftIO)

getData :: FromJSON v => Connection -> ByteString -> IO (Maybe v)
getData conn k = runRedis conn $ either (const Nothing) (maybe Nothing decodeStrict) <$> get k

setData :: ToJSON v => Connection -> ByteString -> v -> IO ()
setData conn k v = runRedis conn . void . set k . toStrict $ encode v

delData :: Connection -> [ByteString] -> IO ()
delData conn ks = runRedis conn . void $ del ks

-- | Return the cached result of the action or, in the case of a cache
-- miss, execute the action and insert it in the cache.
cached :: (FromJSON v, ToJSON v) => (u -> Maybe Connection) -> ByteString -> GenHaxl u (Maybe v) -> GenHaxl u (Maybe v)
cached redis k io = do
  h <- redis <$> env userEnv
  go h k io

  where go :: (FromJSON v, ToJSON v) => Maybe Connection -> ByteString -> GenHaxl u (Maybe v) -> GenHaxl u (Maybe v)
        go Nothing _ io0 = io0
        go (Just conn) k0 io0 = do
          res <- unsafeLiftIO $ getData conn k0
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
          res <- unsafeLiftIO $ getData conn k0
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
