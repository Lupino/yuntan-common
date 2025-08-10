{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Database.PSQL.Types.From
  ( From (..)
  ) where

import           Data.Hashable (Hashable (..))
import           Data.Int      (Int64)
import           GHC.Generics  (Generic)

newtype From = From { unFrom :: Int64 }
  deriving (Generic, Eq, Num, Integral, Real, Ord, Enum)
instance Show From where
  show = getFrom

instance Hashable From


getFrom :: From -> String
getFrom (From 0) = ""
getFrom s        = " OFFSET " ++ show s
