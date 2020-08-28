{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Database.PSQL.Config
  ( PSQL (..)
  , genPSQLPool
  ) where

import           Data.Aeson                 (FromJSON, parseJSON, withObject,
                                             (.!=), (.:), (.:?))
import           Data.Pool                  (Pool, createPool)
import           Data.Time                  (NominalDiffTime)
import           Database.PostgreSQL.Simple (ConnectInfo (..), Connection,
                                             close, connect, defaultConnectInfo)
import           GHC.Word                   (Word16)

data PSQL = PSQL
    { psqlDBName           :: String
    , psqlHost             :: String
    , psqlPort             :: Word16
    , psqlUser             :: String
    , psqlPass             :: String
    , psqlPoolNumStrips    :: Int
    -- ^ The number of stripes (distinct sub-pools) to maintain.
    , psqlPoolIdleTime     :: NominalDiffTime
    -- ^ Amount of time for which an unused resource is kept alive.
    , psqlPoolMaxResources :: Int
    -- ^ Maximum number of resources to maintain per stripe.  The
    , psqlHaxlNumThreads   :: Int
    -- numThreads of fetch async for haxl
    }
    deriving (Show)

instance FromJSON PSQL where
  parseJSON = withObject "PSQL" $ \o -> do
    psqlDBName           <- o .:  "db"
    psqlHost             <- o .:? "host"         .!= "127.0.0.1"
    psqlPort             <- o .:? "port"         .!= 5432
    psqlUser             <- o .:? "user"         .!= "postgre"
    psqlPass             <- o .:? "pass"         .!= ""
    psqlPoolNumStrips    <- o .:? "numStripes"   .!= 1
    psqlPoolIdleTime     <- o .:? "idleTime"     .!= 0.5
    psqlPoolMaxResources <- o .:? "maxResources" .!= 1
    psqlHaxlNumThreads   <- o .:? "numThreads"   .!= 1
    return PSQL{..}

genPSQLPool :: PSQL -> IO (Pool Connection)
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
