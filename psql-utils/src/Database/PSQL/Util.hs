{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Database.PSQL.Util
  ( getOnly
   , getOnlyDefault
  ) where


import           Data.Maybe                         (listToMaybe)
import           Database.PostgreSQL.Simple         (Only (..))
import           Database.PostgreSQL.Simple.FromRow (FromRow (..))

getOnly :: FromRow (Only a) => [Only a] -> Maybe a
getOnly = fmap fromOnly . listToMaybe

getOnlyDefault :: FromRow (Only a) => a -> [Only a] -> a
getOnlyDefault a = maybe a fromOnly . listToMaybe
