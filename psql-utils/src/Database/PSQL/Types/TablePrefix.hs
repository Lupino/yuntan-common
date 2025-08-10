module Database.PSQL.Types.TablePrefix
  ( TablePrefix
  , getPrefix
  ) where

import           Data.String (IsString (..))

newtype TablePrefix = TablePrefix String
  deriving (Show)

instance IsString TablePrefix where
  fromString = TablePrefix

getPrefix :: TablePrefix -> String
getPrefix (TablePrefix "") = ""
getPrefix (TablePrefix x)  = x ++ "_"

