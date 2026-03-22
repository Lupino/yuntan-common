{-# LANGUAGE OverloadedStrings #-}

module Database.PSQL.Version
  ( mergeDatabase
  , VersionList
  ) where

import           Control.Monad              (foldM_, void)
import           Data.Int                   (Int64)
import           Data.List                  (sortOn)
import           Database.PostgreSQL.Simple (Only (..))
import           Database.PSQL.Exc          (createTable, insertRet, update)
import           Database.PSQL.Select       (selectOneOnly)
import           Database.PSQL.Types.PSQL   (PSQL, withTransaction)

createVersionTable :: PSQL Int64
createVersionTable =
  createTable "version"
    [ "name VARCHAR(10) NOT NULL"
    , "version INT DEFAULT 0"
    , "PRIMARY KEY (name)"
    ]

getCurrentVersion :: PSQL Int64
getCurrentVersion = do
  void createVersionTable
  ts <- selectOneOnly "version" "version" "name = ?" (Only ("version" :: String))
  case ts of
    Just v -> pure v
    Nothing  ->
      insertRet "version" ["name", "version"] "version" ("version" :: String, 0 :: Int) 0


updateVersion :: Int64 -> PSQL ()
updateVersion ts =
  void $ update "version" ["version"] "name = ?" (ts, "version" :: String)

type Version a = (Int64, [PSQL a])
type VersionList a = [Version a]

mergeDatabase :: VersionList a -> PSQL ()
mergeDatabase versionList = withTransaction $ do
  version <- getCurrentVersion
  foldM_ processAction version (sortOn fst versionList)

processAction :: Int64 -> Version a -> PSQL Int64
processAction version (ts, actions)
  | ts > version = do
    mapM_ void actions
    updateVersion ts
    pure ts
  | otherwise = pure version
