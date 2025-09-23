{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Database.PSQL.Config
  ( PSQLConfig (..)
  , genPSQLPool
  ) where

import           Data.Aeson                 (FromJSON, parseJSON, withObject,
                                             (.!=), (.:), (.:?))
import           Data.Pool                  (Pool, defaultPoolConfig, newPool)
import           Database.PostgreSQL.Simple (ConnectInfo (..), Connection,
                                             close, connect, defaultConnectInfo)
import           GHC.Word                   (Word16)

data PSQLConfig = PSQLConfig
    { psqlDBName           :: String
    , psqlHost             :: String
    , psqlPort             :: Word16
    , psqlUser             :: String
    , psqlPass             :: String
    , psqlPoolIdleTime     :: Double
    -- ^ Amount of time for which an unused resource is kept alive.
    , psqlPoolMaxResources :: Int
    -- ^ The maximum number of resources to keep open across all stripes.
    -- The smallest acceptable value is 1 per stripe.

    -- Note: if the number of stripes does not divide the number of resources,
    -- some of the stripes will have 1 more resource available than the others.
    }
    deriving (Show)

instance FromJSON PSQLConfig where
  parseJSON = withObject "PSQLConfig" $ \o -> do
    psqlDBName           <- o .:  "db"
    psqlHost             <- o .:? "host"         .!= "127.0.0.1"
    psqlPort             <- o .:? "port"         .!= 5432
    psqlUser             <- o .:? "user"         .!= "postgres"
    psqlPass             <- o .:? "pass"         .!= ""
    psqlPoolIdleTime     <- o .:? "idleTime"     .!= 0.5
    psqlPoolMaxResources <- o .:? "maxResources" .!= 1
    return PSQLConfig{..}

genPSQLPool :: PSQLConfig -> IO (Pool Connection)
genPSQLPool conf =
  newPool $ defaultPoolConfig conn close idleTime maxResources
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

        idleTime     = psqlPoolIdleTime     conf
        maxResources = psqlPoolMaxResources conf
