{-# LANGUAGE OverloadedStrings #-}

module Yuntan.Types.HasMySQL
  (
    TablePrefix
  , MySQL
  , HasMySQL
  , mysqlPool
  , tablePrefix
  , SimpleEnv
  , simpleEnv

  , VersionList
  , mergeDatabase
  ) where


import           Data.Pool             (Pool)
import           Database.MySQL.Simple (Connection, Only (..), execute_, query_)

import           Control.Monad         (void)
import           Data.Int              (Int64)
import           Data.Maybe            (listToMaybe)
import           Data.String           (fromString)

type TablePrefix = String

type MySQL a = TablePrefix -> Connection -> IO a

class HasMySQL u where
  mysqlPool   :: u -> Pool Connection
  tablePrefix :: u -> TablePrefix

data SimpleEnv = SimpleEnv { pc :: Pool Connection
                           , pf :: String
                           }

instance HasMySQL SimpleEnv where
  mysqlPool = pc
  tablePrefix = pf

simpleEnv :: Pool Connection -> TablePrefix -> SimpleEnv
simpleEnv pool prefix = SimpleEnv{pc=pool, pf = prefix}

createVersionTable :: MySQL ()
createVersionTable prefix conn = void $ execute_ conn sql
  where sql = fromString $ concat [ "CREATE TABLE IF NOT EXISTS `", prefix, "_version` ("
                                  , "  `name` varchar(128) NOT NULL,"
                                  , "  `version` int(10) unsigned DEFAULT '0',"
                                  , "  PRIMARY KEY (`name`)"
                                  , ") ENGINE=InnoDB DEFAULT CHARSET=utf8"
                                  ]

getCurrentVersion :: MySQL Int64
getCurrentVersion prefix conn = do
  void $ createVersionTable prefix conn
  ts <- query_ conn $ fromString $ concat
    [ "SELECT `version` FROM `", prefix, "_version`"
    , " WHERE `name` = 'version'"
    ]
  case listToMaybe ts of
    Just ts' -> pure (fromOnly ts')
    Nothing  -> do
      void $ execute_ conn $ fromString $ concat
        [ "INSERT INTO `", prefix, "_version`"
        , " (`name`, `version`)"
        , " VALUES"
        , " ('version', 0)"
        ]
      pure 0


updateVersion :: Int64 -> MySQL ()
updateVersion ts prefix conn =
  void $ execute_ conn $ fromString $ concat
    [ "UPDATE `", prefix, "_version`"
    , " SET `version` = ", show ts
    , " WHERE `name` = 'version'"
    ]

type Version = (Int64, [MySQL ()])
type VersionList = [Version]

mergeDatabase :: VersionList -> MySQL ()
mergeDatabase versionList prefix conn = do
  version <- getCurrentVersion prefix conn
  mapM_ (\v -> processAction version v prefix conn) versionList

processAction :: Int64 -> Version -> MySQL ()
processAction version (ts, actions) prefix conn =
  if ts > version then do
                  updateVersion ts prefix conn
                  mapM_ (\o -> o prefix conn) actions
                  else pure ()
