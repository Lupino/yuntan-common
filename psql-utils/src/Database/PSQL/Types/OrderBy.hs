{-# LANGUAGE DeriveGeneric #-}

module Database.PSQL.Types.OrderBy
  ( OrderBy
  , desc
  , asc
  , orderNone
  ) where

import           Data.Hashable (Hashable (..))
import           GHC.Generics  (Generic)

data OrderBy = Desc String | Asc String | OrderNone
  deriving (Generic, Eq)

instance Hashable OrderBy

desc :: String -> OrderBy
desc = Desc

asc :: String -> OrderBy
asc = Asc

orderNone :: OrderBy
orderNone = OrderNone

instance Show OrderBy where
  show (Desc f)  = " ORDER BY " ++ f ++ " DESC"
  show (Asc f)   = " ORDER BY " ++ f ++ " ASC"
  show OrderNone = ""

