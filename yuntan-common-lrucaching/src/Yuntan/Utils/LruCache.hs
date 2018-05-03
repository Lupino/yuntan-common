module Yuntan.Utils.LruCache
  ( LruHandle (..)
  , newLruHandle
  , cached'
  , cached
  , remove
  , updateLruHandle
  ) where


import           Data.Hashable          (Hashable (..))
import           Haxl.Core              (GenHaxl, env, userEnv)
import           Haxl.Core.Monad        (unsafeLiftIO)

import           Data.HashPSQ           (delete, member)
import           Data.IORef             (atomicModifyIORef')
import           Data.LruCache          (empty, insert, lookup)
import           Data.LruCache.Internal (LruCache (..))
import           Data.LruCache.IO       (LruHandle (..), newLruHandle)
import           Prelude                hiding (lookup)

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

updateLruHandle :: (Hashable k, Ord k) => LruHandle k v -> Int -> IO ()
updateLruHandle (LruHandle ref) size =
  atomicModifyIORef' ref $ const (empty size, ())
