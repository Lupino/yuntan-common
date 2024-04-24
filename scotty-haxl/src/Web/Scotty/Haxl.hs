module Web.Scotty.Haxl
  ( ActionH
  , ScottyH
  ) where


import           Control.Monad.IO.Class  (MonadIO (..))
import           Control.Monad.IO.Unlift (MonadUnliftIO (..))
import           Haxl.Core               (runHaxl)
import           Haxl.Core.Monad         (GenHaxl (..), Result (..),
                                          unsafeLiftIO)
import           Web.Scotty.Trans        (ActionT, ScottyT)


type ActionH u w b = ActionT (GenHaxl u w) b
type ScottyH u w b = ScottyT (GenHaxl u w) b

instance MonadIO (GenHaxl u w) where
  liftIO = unsafeLiftIO


instance Monoid w => MonadUnliftIO (GenHaxl u w) where
  withRunInIO inner =
    GenHaxl $ \env ->
      Done <$> inner (runHaxl env)
