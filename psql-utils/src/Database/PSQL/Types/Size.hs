{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Database.PSQL.Types.Size
  ( Size (..)
  ) where

import           Data.Hashable (Hashable (..))
import           Data.Int      (Int64)
import           GHC.Generics  (Generic)

newtype Size = Size { unSize :: Int64 }
  deriving (Generic, Eq, Num, Integral, Real, Ord, Enum)

instance Show Size where
  show = getSize

instance Hashable Size


getSize :: Size -> String
getSize (Size 0) = ""
getSize (Size s) = " LIMIT " ++ show s
