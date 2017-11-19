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

type ActionH u b = ActionT LT.Text (GenHaxl u) b
type ScottyH u b = ScottyT LT.Text (GenHaxl u) b

instance MonadIO (GenHaxl u) where
  liftIO = unsafeLiftIO

