{-# LANGUAGE DeriveGeneric #-}

module Database.PSQL.Types.GroupBy
  ( GroupBy
  , group
  , groupNone
  ) where

import           Data.Hashable (Hashable (..))
import           GHC.Generics  (Generic)

data GroupBy = GroupBy String | GroupNone
  deriving (Generic, Eq)

instance Hashable GroupBy

group :: String -> GroupBy
group = GroupBy

groupNone :: GroupBy
groupNone = GroupNone

instance Show GroupBy where
  show (GroupBy f) = " GROUP BY " ++ f
  show GroupNone   = ""

