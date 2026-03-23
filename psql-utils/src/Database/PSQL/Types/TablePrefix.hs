module Database.PSQL.Types.TablePrefix
  ( TablePrefix
  , getPrefix
  ) where

import           Data.Char   (isSpace)
import           Data.String (IsString (..))

newtype TablePrefix = TablePrefix String
  deriving (Show)

instance IsString TablePrefix where
  fromString = TablePrefix

getPrefix :: TablePrefix -> String
getPrefix (TablePrefix "") = ""
getPrefix (TablePrefix x)
  | all isSpace x = error "getPrefix: invalid prefix"
  | hasOuterSpace x = error "getPrefix: invalid prefix"
  | hasQuote x = error "getPrefix: quoted identifiers are not supported"
  | last x == '_' = x
  | otherwise = x ++ "_"

hasQuote :: String -> Bool
hasQuote = elem '"'

hasOuterSpace :: String -> Bool
hasOuterSpace [] = False
hasOuterSpace [c] = isSpace c
hasOuterSpace s@(c:_) = isSpace c || isSpace (last s)
