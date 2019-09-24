module Yuntan.Types.Scotty
  (
    ActionH
  , ScottyH
  ) where

import           Control.Monad.IO.Class (MonadIO (..))
import           Haxl.Core              (GenHaxl)
import           Haxl.Core.Monad        (unsafeLiftIO)
import           Web.Scotty.Trans       (ActionT, ScottyT)

import qualified Data.Text.Lazy         as LT (Text)

type ActionH u w b = ActionT LT.Text (GenHaxl u w) b
type ScottyH u w b = ScottyT LT.Text (GenHaxl u w) b

instance MonadIO (GenHaxl u w) where
  liftIO = unsafeLiftIO

