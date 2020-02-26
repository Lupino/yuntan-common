{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Yuntan.Types.OrderBy
  ( OrderBy
  , asc
  , desc
  , emptyOrder
  , show1
  ) where

import           Data.Hashable (Hashable (..))
import           GHC.Generics  (Generic)

data OrderBy = Desc String | Asc String | EmptyOrder
  deriving (Generic, Eq)

instance Hashable OrderBy

desc :: String -> OrderBy
desc = Desc

asc :: String -> OrderBy
asc = Asc

emptyOrder :: OrderBy
emptyOrder = EmptyOrder

quote :: String -> String
quote s@(x:xs) | '.' `elem` s = x : quote xs
               | otherwise    = '`' : x : xs ++ "`"
quote []                      = []

instance Show OrderBy where
  show (Desc field) = "ORDER BY " ++ quote field ++ " DESC"
  show (Asc field)  = "ORDER BY " ++ quote field ++ " ASC"
  show EmptyOrder   = ""

show1 :: OrderBy -> String
show1 (Desc field) = "ORDER BY " ++ field ++ " DESC"
show1 (Asc field)  = "ORDER BY " ++ field ++ " ASC"
show1 EmptyOrder   = ""
