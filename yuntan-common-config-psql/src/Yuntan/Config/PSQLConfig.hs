{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Yuntan.Config.PSQLConfig
  ( PSQLConfig (..)
  , genPSQLPool
  ) where

import           Data.Aeson                 (FromJSON, parseJSON, withObject,
                                             (.!=), (.:), (.:?))
import           Data.Pool                  (Pool, createPool)
import           Data.Time                  (NominalDiffTime)
import           Database.PostgreSQL.Simple (ConnectInfo (..), Connection,
                                             close, connect, defaultConnectInfo)
import           GHC.Word                   (Word16)

data PSQLConfig = PSQLConfig
  { psqlDBName           :: String
  , psqlHost             :: String
  , psqlPort             :: Word16
  , psqlUser             :: String
  , psqlPass             :: String
  , psqlPoolNumStrips    :: Int
  -- ^ The number of stripes (distinct sub-pools) to maintain.
  -- The smallest acceptable value is 1.
  , psqlPoolIdleTime     :: NominalDiffTime
  -- ^ Amount of time for which an unused resource is kept alive.
  -- The smallest acceptable value is 0.5 seconds.
  --
  -- The elapsed time before closing may be a little longer than
  -- requested, as the reaper thread wakes at 1-second intervals.
  , psqlPoolMaxResources :: Int
  -- ^ Maximum number of resources to maintain per stripe.  The
  -- smallest acceptable value is 1.
  --
  -- Requests for resources will block if this limit is reached on a
  -- single stripe, even if other stripes have idle resources
  -- available.
  , psqlHaxlNumThreads   :: Int
  -- numThreads of fetch async for haxl
  }
  deriving (Show)

instance FromJSON PSQLConfig where
  parseJSON = withObject "PSQLConfig" $ \o -> do
    psqlDBName           <- o .:  "db"
    psqlHost             <- o .:? "host"         .!= "127.0.0.1"
    psqlPort             <- o .:? "port"         .!= 5432
    psqlUser             <- o .:? "user"         .!= "postgre"
    psqlPass             <- o .:? "pass"         .!= ""
    psqlPoolNumStrips    <- o .:? "numStripes"   .!= 1
    psqlPoolIdleTime     <- o .:? "idleTime"     .!= 0.5
    psqlPoolMaxResources <- o .:? "maxResources" .!= 1
    psqlHaxlNumThreads   <- o .:? "numThreads"   .!= 1
    return PSQLConfig{..}

genPSQLPool :: PSQLConfig -> IO (Pool Connection)
genPSQLPool conf = createPool conn close numStripes idleTime maxResources
  where conn = connect defaultConnectInfo
                  { connectDatabase = dbName
                  , connectHost     = dbHost
                  , connectPort     = dbPort
                  , connectUser     = dbUser
                  , connectPassword = dbPass
                  }

        dbName       = psqlDBName conf
        dbHost       = psqlHost   conf
        dbPort       = psqlPort   conf
        dbUser       = psqlUser   conf
        dbPass       = psqlPass   conf

        numStripes   = psqlPoolNumStrips    conf
        idleTime     = psqlPoolIdleTime     conf
        maxResources = psqlPoolMaxResources conf
