module Dispatch.Utils.Haxl
  (
    removeCache
  , clearCache
  , runWithEnv
  ) where

import           Data.HashMap.Strict (delete)
import           Data.IORef          (readIORef, writeIORef)
import           Data.Typeable       (Typeable, typeOf)
import           Haxl.Core           (Env (..), GenHaxl (..), env, initEnv,
                                      withEnv)
import           Haxl.Core.Monad     (unsafeLiftIO)
import           Haxl.Core.Types     (DataCache (..), emptyDataCache)

removeCache :: Typeable k => k -> GenHaxl u ()
removeCache k = do
  ref <- env cacheRef
  unsafeLiftIO $ writeIORef ref . doDel =<< readIORef ref

  where doDel :: DataCache u -> DataCache u
        doDel (DataCache cache) = DataCache $ delete key cache

        key = typeOf k

clearCache :: GenHaxl u ()
clearCache = do
  ref <- env cacheRef
  unsafeLiftIO $ writeIORef ref emptyDataCache

runWithEnv :: GenHaxl u a -> GenHaxl u a
runWithEnv act = do
  state <- env states
  ue <- env userEnv
  env0 <- unsafeLiftIO $ initEnv state ue
  withEnv env0 act
