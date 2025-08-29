{-# LANGUAGE DeriveDataTypeable       #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE TypeFamilies             #-}

module Haxl.RedisCache
  ( cached
  , cached'
  , remove
  , initRedisState

  , get
  , set
  , del

  , hget
  , hset
  , hdel

  , hexists
  , hgetall

  , genKey
  ) where

import           Control.Concurrent.Async
import           Control.Concurrent.QSem
import qualified Control.Exception        as CE (SomeException, bracket_, try)
import           Control.Monad            (void)
import           Data.Aeson               (FromJSON, ToJSON, decodeStrict,
                                           encode)
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as B (concat)
import           Data.ByteString.Lazy     (toStrict)
import           Data.Either              (fromRight)
import           Data.Hashable            (Hashable (..))
import           Data.List                (groupBy)
import           Data.Typeable            (Typeable)
import           Database.Redis           (Connection, runRedis)
import qualified Database.Redis           as R (del, get, hdel, hexists, hget,
                                                hgetall, hmget, hmset, hset,
                                                mget, mset, set)
import           Haxl.Core                hiding (fetchReq)

newtype Conn = Conn Connection

instance Eq Conn where
  _ == _ = True

instance Show Conn where
  show _ = "Conn"

genKey_ :: ByteString -> ByteString -> ByteString
genKey_ pref k = B.concat [pref, ":", k]

getData_ :: Connection -> ByteString -> IO (Maybe ByteString)
getData_ conn k = runRedis conn $ fromRight Nothing <$> R.get k

mGetData_ :: Connection -> [ByteString] -> IO [Maybe ByteString]
mGetData_ conn ks = runRedis conn $ fromRight [] <$> R.mget ks

setData_ :: Connection -> ByteString -> ByteString -> IO ()
setData_ conn k = runRedis conn . void . R.set k

mSetData_ :: Connection -> [(ByteString, ByteString)] -> IO ()
mSetData_ conn = runRedis conn . void . R.mset

delData_ :: Connection -> [ByteString] -> IO ()
delData_ conn ks = runRedis conn . void $ R.del ks

hgetData_ :: Connection -> ByteString -> ByteString -> IO (Maybe ByteString)
hgetData_ conn k f = runRedis conn $ fromRight Nothing <$> R.hget k f

hmGetData_ :: Connection -> ByteString -> [ByteString] -> IO [Maybe ByteString]
hmGetData_ conn k fs = runRedis conn $ fromRight [] <$> R.hmget k fs

hsetData_ :: Connection -> ByteString -> ByteString -> ByteString -> IO ()
hsetData_ conn k f = runRedis conn . void . R.hset k f

hmSetData_ :: Connection -> ByteString -> [(ByteString, ByteString)] -> IO ()
hmSetData_ conn k = runRedis conn . void . R.hmset k

hdelData_ :: Connection -> ByteString -> [ByteString] -> IO ()
hdelData_ conn k fs = runRedis conn . void $ R.hdel k fs

hgetallData_ :: Connection -> ByteString -> IO [(ByteString, ByteString)]
hgetallData_ conn k = runRedis conn $ fromRight [] <$> R.hgetall k

hexistsData_ :: Connection -> ByteString -> ByteString -> IO Bool
hexistsData_ conn k f = runRedis conn $ fromRight False <$> R.hexists k f

-- Data source implementation.

data RedisReq a where
  GetData :: Conn -> ByteString -> RedisReq (Maybe ByteString)
  SetData :: Conn -> ByteString -> ByteString -> RedisReq ()
  DelData :: Conn -> [ByteString] -> RedisReq ()
  HGetData :: Conn -> ByteString -> ByteString -> RedisReq (Maybe ByteString)
  HSetData :: Conn -> ByteString -> ByteString -> ByteString -> RedisReq ()
  HDelData :: Conn -> ByteString -> [ByteString] -> RedisReq ()

  HGetAllData :: Conn -> ByteString -> RedisReq [(ByteString, ByteString)]
  HExistsData :: Conn -> ByteString -> ByteString -> RedisReq Bool

  GenKey  :: ByteString -> RedisReq ByteString
  deriving (Typeable)

deriving instance Eq (RedisReq a)
instance Hashable (RedisReq a) where
  hashWithSalt s (GetData _ k)       = hashWithSalt s (1::Int, k)
  hashWithSalt s (SetData _ k v)     = hashWithSalt s (2::Int, k, v)
  hashWithSalt s (DelData _ ks)      = hashWithSalt s (3::Int, ks)
  hashWithSalt s (HGetData _ k f)    = hashWithSalt s (4::Int, k, f)
  hashWithSalt s (HSetData _ k f v)  = hashWithSalt s (5::Int, k, f, v)
  hashWithSalt s (HDelData _ k fs)   = hashWithSalt s (6::Int, k, fs)

  hashWithSalt s (HGetAllData _ k)   = hashWithSalt s (7::Int, k)
  hashWithSalt s (HExistsData _ k f) = hashWithSalt s (8::Int, k, f)

  hashWithSalt s (GenKey k)          = hashWithSalt s (9::Int, k)

deriving instance Show (RedisReq a)
instance ShowP RedisReq where showp = show

instance StateKey RedisReq where
  data State RedisReq = RedisState { numThreads :: Int, redisPrefix :: ByteString }

instance DataSourceName RedisReq where
  dataSourceName _ = "RedisDataSource"

instance DataSource u RedisReq where
  fetch = doFetch

isSameType :: BlockedFetch RedisReq -> BlockedFetch RedisReq -> Bool
isSameType (BlockedFetch (GetData {}) _) (BlockedFetch (GetData {}) _)               = True
isSameType (BlockedFetch (SetData {}) _) (BlockedFetch (SetData {}) _)               = True
isSameType (BlockedFetch (DelData {}) _) (BlockedFetch (DelData {}) _)               = True
isSameType (BlockedFetch (HGetData _ k0 _) _) (BlockedFetch (HGetData _ k1 _) _)     = k0    == k1
isSameType (BlockedFetch (HSetData _ k0 _ _) _) (BlockedFetch (HSetData _ k1 _ _) _) = k0    == k1
isSameType (BlockedFetch (HDelData _ k0 _) _) (BlockedFetch (HDelData _ k1 _) _)     = k0    == k1
isSameType _ _                                                                       = False

doFetch
  :: State RedisReq
  -> Flags
  -> u
  -> PerformFetch RedisReq

doFetch _state _flags _ = AsyncFetch $ \reqs inner -> do
  sem <- newQSem $ numThreads _state
  asyncs <- mapM (fetchAsync sem (redisPrefix _state)) (groupBy isSameType reqs)
  inner
  mapM_ wait asyncs

fetchAsync :: QSem -> ByteString -> [BlockedFetch RedisReq] -> IO (Async ())
fetchAsync sem pref reqs = async $
  CE.bracket_ (waitQSem sem) (signalQSem sem) $ fetchSync pref reqs

runVoid :: [ResultVar ()] -> IO () -> IO ()
runVoid rvars io = do
  e <- CE.try io
  case e of
    Left ex -> mapM_ (`putFailure` (ex :: CE.SomeException)) rvars
    Right _ -> mapM_ (`putSuccess` ()) rvars

fetchSync :: ByteString -> [BlockedFetch RedisReq] -> IO ()
fetchSync _ [] = return ()
fetchSync pref [BlockedFetch req rvar] = do
  e <- CE.try $ fetchReq pref req
  case e of
    Left ex -> putFailure rvar (ex :: CE.SomeException)
    Right a -> putSuccess rvar a
fetchSync pref reqs@((BlockedFetch (GetData (Conn conn) _) _):_) = do
  e <- CE.try $ mGetData_ conn ids
  case e of
    Left ex -> mapM_ (`putFailure` (ex :: CE.SomeException)) rvars
    Right a -> mapM_ (uncurry putSuccess) $ zip rvars a

  where ids = [genKey_ pref i | BlockedFetch (GetData _ i) _ <- reqs]
        rvars = [rvar | BlockedFetch (GetData _ _) rvar <- reqs]

fetchSync pref reqs@((BlockedFetch (SetData (Conn conn) _ _) _):_) =
  runVoid rvars $ mSetData_ conn kvs

  where kvs = [(genKey_ pref i, v) | BlockedFetch (SetData _ i v) _ <- reqs]
        rvars = [rvar | BlockedFetch (SetData {}) rvar <- reqs]

fetchSync pref reqs@((BlockedFetch (DelData (Conn conn) _) _):_) =
  runVoid rvars $ delData_ conn $ concat ks
  where ks = [map (genKey_ pref) i | BlockedFetch (DelData _ i) _ <- reqs]
        rvars = [rvar | BlockedFetch (DelData _ _) rvar <- reqs]

fetchSync pref reqs@((BlockedFetch (HGetData (Conn conn) k _) _):_) = do
  e <- CE.try $ hmGetData_ conn (genKey_ pref k) ids
  case e of
    Left ex -> mapM_ (`putFailure` (ex :: CE.SomeException)) rvars
    Right a -> mapM_ (uncurry putSuccess) $ zip rvars a

  where ids = [i | BlockedFetch (HGetData _ _ i) _ <- reqs]
        rvars = [rvar | BlockedFetch (HGetData {}) rvar <- reqs]

fetchSync pref reqs@((BlockedFetch (HSetData (Conn conn) k _ _) _):_) =
  runVoid rvars $ hmSetData_ conn (genKey_ pref k) fvs

  where fvs = [(i, v) | BlockedFetch (HSetData _ _ i v) _ <- reqs]
        rvars = [rvar | BlockedFetch (HSetData {}) rvar <- reqs]

fetchSync pref reqs@((BlockedFetch (HDelData (Conn conn) k _) _):_) =
  runVoid rvars $ hdelData_ conn (genKey_ pref k) $ concat fs
  where fs = [i | BlockedFetch (HDelData _ _ i) _ <- reqs]
        rvars = [rvar | BlockedFetch (HDelData {}) rvar <- reqs]

fetchSync pref xs = mapM_ (\x -> fetchSync pref [x]) xs

fetchReq :: ByteString -> RedisReq a -> IO a
fetchReq pref (GetData (Conn conn) k)       = getData_ conn $ genKey_ pref k
fetchReq pref (SetData (Conn conn) k v)     = setData_ conn (genKey_ pref k) v
fetchReq pref (DelData (Conn conn) ks)      = delData_ conn $ map (genKey_ pref) ks
fetchReq pref (HGetData (Conn conn) k f)    = hgetData_ conn (genKey_ pref k) f
fetchReq pref (HSetData (Conn conn) k f v)  = hsetData_ conn (genKey_ pref k) f v
fetchReq pref (HDelData (Conn conn) k fs)   = hdelData_ conn (genKey_ pref k) fs
fetchReq pref (HGetAllData (Conn conn) k)   = hgetallData_ conn (genKey_ pref k)
fetchReq pref (HExistsData (Conn conn) k f) = hexistsData_ conn (genKey_ pref k) f
fetchReq pref (GenKey k)                    = return $ genKey_ pref k

initRedisState :: Int -> ByteString -> State RedisReq
initRedisState = RedisState

getData :: FromJSON v => Connection -> ByteString -> GenHaxl u w (Maybe v)
getData conn k = maybe Nothing decodeStrict <$> dataFetch (GetData (Conn conn) k)

setData :: ToJSON v => Connection -> ByteString -> v -> GenHaxl u w ()
setData conn k v = uncachedRequest . SetData (Conn conn) k . toStrict $ encode v

delData :: Connection -> [ByteString] -> GenHaxl u w ()
delData conn = uncachedRequest . DelData (Conn conn)

hgetData :: FromJSON v => Connection -> ByteString -> ByteString -> GenHaxl u w (Maybe v)
hgetData conn k f = maybe Nothing decodeStrict <$> dataFetch (HGetData (Conn conn) k f)

hsetData :: ToJSON v => Connection -> ByteString -> ByteString -> v -> GenHaxl u w ()
hsetData conn k f v = uncachedRequest . HSetData (Conn conn) k f . toStrict $ encode v

hdelData :: Connection -> ByteString -> [ByteString] -> GenHaxl u w ()
hdelData conn k = uncachedRequest . HDelData (Conn conn) k

hgetallData :: Connection -> ByteString -> GenHaxl u w [(ByteString, ByteString)]
hgetallData conn k = dataFetch (HGetAllData (Conn conn) k)

hexistsData :: Connection -> ByteString -> ByteString -> GenHaxl u w Bool
hexistsData conn k f = dataFetch (HExistsData (Conn conn) k f)

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
remove redis k = del redis [k]

del :: (u -> Maybe Connection) -> [ByteString] -> GenHaxl u w ()
del redis k = do
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

hdel :: (u -> Maybe Connection) -> ByteString -> [ByteString] -> GenHaxl u w ()
hdel redis k fs = do
  h <- redis <$> env userEnv
  case h of
    Nothing   -> return ()
    Just conn -> hdelData conn k fs

hget :: FromJSON v => (u -> Maybe Connection) -> ByteString -> ByteString -> GenHaxl u w (Maybe v)
hget redis k f = do
  h <- redis <$> env userEnv
  case h of
    Nothing   -> return Nothing
    Just conn -> hgetData conn k f

hset :: ToJSON v => (u -> Maybe Connection) -> ByteString -> ByteString -> v -> GenHaxl u w ()
hset redis k f v = do
  h <- redis <$> env userEnv
  case h of
    Nothing   -> return ()
    Just conn -> hsetData conn k f v

hgetall :: (u -> Maybe Connection) -> ByteString -> GenHaxl u w [(ByteString, ByteString)]
hgetall redis k = do
  h <- redis <$> env userEnv
  case h of
    Nothing   -> return []
    Just conn -> hgetallData conn k

hexists :: (u -> Maybe Connection) -> ByteString -> ByteString -> GenHaxl u w Bool
hexists redis k f = do
  h <- redis <$> env userEnv
  case h of
    Nothing   -> return False
    Just conn -> hexistsData conn k f

genKey :: ByteString -> GenHaxl v w ByteString
genKey = dataFetch . GenKey
