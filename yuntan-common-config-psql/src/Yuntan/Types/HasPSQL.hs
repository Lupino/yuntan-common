{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module Yuntan.Types.HasPSQL
  ( TablePrefix
  , PSQL
  , HasPSQL
  , psqlPool
  , tablePrefix
  , SimpleEnv
  , simpleEnv

  , HasOtherEnv
  , otherEnv

  , VersionList
  , mergeDatabase
  ) where


import           Control.Monad              (void)
import           Data.Int                   (Int64)
import           Data.Maybe                 (listToMaybe)
import           Data.Pool                  (Pool)
import           Data.String                (fromString)
import           Database.PostgreSQL.Simple (Connection, Only (..), execute_,
                                             query_)


type TablePrefix = String

type PSQL a = TablePrefix -> Connection -> IO a

class HasPSQL u where
  psqlPool   :: u -> Pool Connection
  tablePrefix :: u -> TablePrefix

class HasOtherEnv u a where
  otherEnv :: a -> u

data SimpleEnv u = SimpleEnv
  { pc :: Pool Connection
  , pf :: String
  , pu :: u
  }

instance HasPSQL (SimpleEnv u) where
  psqlPool = pc
  tablePrefix = pf

instance HasOtherEnv u (SimpleEnv u) where
  otherEnv = pu

simpleEnv :: Pool Connection -> TablePrefix -> u -> SimpleEnv u
simpleEnv pool prefix env0 = SimpleEnv{pc=pool, pf = prefix, pu = env0}

createVersionTable :: PSQL ()
createVersionTable prefix conn = void $ execute_ conn sql
  where sql = fromString $ concat
          [ "CREATE TABLE IF NOT EXISTS ", prefix, "_version ("
          , "  name CHAR(10) NOT NULL,"
          , "  version INT DEFAULT '0',"
          , "  PRIMARY KEY (name)"
          , ")"
          ]

getCurrentVersion :: PSQL Int64
getCurrentVersion prefix conn = do
  void $ createVersionTable prefix conn
  ts <- query_ conn $ fromString $ concat
    [ "SELECT version FROM ", prefix, "_version"
    , " WHERE name = 'version'"
    ]
  case listToMaybe ts of
    Just ts' -> pure (fromOnly ts')
    Nothing  -> do
      void $ execute_ conn $ fromString $ concat
        [ "INSERT INTO ", prefix, "_version"
        , " (name, version)"
        , " VALUES"
        , " ('version', 0)"
        ]
      pure 0


updateVersion :: Int64 -> PSQL ()
updateVersion ts prefix conn =
  void $ execute_ conn $ fromString $ concat
    [ "UPDATE ", prefix, "_version"
    , " SET version = ", show ts
    , " WHERE name = 'version'"
    ]

type Version = (Int64, [PSQL ()])
type VersionList = [Version]

mergeDatabase :: VersionList -> PSQL ()
mergeDatabase versionList prefix conn = do
  version <- getCurrentVersion prefix conn
  mapM_ (\v -> processAction version v prefix conn) versionList

processAction :: Int64 -> Version -> PSQL ()
processAction version (ts, actions) prefix conn =
  if ts > version then do
                  updateVersion ts prefix conn
                  mapM_ (\o -> o prefix conn) actions
                  else pure ()
