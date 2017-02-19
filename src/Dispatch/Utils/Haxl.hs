module Dispatch.Utils.Haxl
  (
    removeCache
  , clearCache
  ) where

import           Data.HashMap.Strict (delete)
import           Data.IORef          (readIORef, writeIORef)
import           Data.Typeable       (Typeable, typeOf)
import           Haxl.Core           (Env (..), GenHaxl (..), env)
import           Haxl.Core.Monad     (unsafeLiftIO)
import           Haxl.Core.Types     (DataCache (..), emptyDataCache)

removeCache :: Typeable k => k -> GenHaxl u ()
removeCache k = do
  ref <- env cacheRef
  unsafeLiftIO $ writeIORef ref . del k  =<< readIORef ref

  where del :: Typeable k => k -> DataCache u -> DataCache u
        del k (DataCache cache) = DataCache $ delete key cache
          where key = typeOf k

clearCache :: GenHaxl u ()
clearCache = do
  ref <- env cacheRef
  unsafeLiftIO $ writeIORef ref emptyDataCache
