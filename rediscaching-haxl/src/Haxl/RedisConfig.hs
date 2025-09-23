{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Haxl.RedisConfig
  ( RedisConfig (..)
  , genRedisConnection
  , defaultRedisConfig
  ) where
import           Data.Aeson     (FromJSON, parseJSON, withObject, (.!=), (.:?))
import           Data.String    (fromString)
import           Data.Time      (NominalDiffTime)
import           Database.Redis (ConnectInfo (..), Connection,
                                 PortID (PortNumber), connect,
                                 defaultConnectInfo)




data RedisConfig = RedisConfig
  { redisHost           :: String
  , redisPort           :: Int
  , redisAuth           :: String
  , redisDB             :: Integer
  -- ^ Each connection will 'select' the database with the given index.
  , redisMaxConnections :: Int
  -- ^ Maximum number of connections to keep open. The smallest acceptable
  --   value is 1.
  , redisMaxIdleTime    :: NominalDiffTime
  -- ^ Amount of time for which an unused connection is kept open. The
  --   smallest acceptable value is 0.5 seconds. If the @timeout@ value in
  --   your redis.conf file is non-zero, it should be larger than
  --   'redisMaxIdleTime'.
  , redisEnable         :: Bool
  }
  deriving (Show)

instance FromJSON RedisConfig where
  parseJSON = withObject "RedisConfig" $ \o -> do
    redisDB             <- o .:?  "db"            .!= 0
    redisHost           <- o .:? "host"           .!= "127.0.0.1"
    redisPort           <- o .:? "port"           .!= 6379
    redisAuth           <- o .:? "auth"           .!= ""
    redisEnable         <- o .:? "enable"         .!= False
    redisMaxConnections <- o .:? "maxConnections" .!= 50
    redisMaxIdleTime    <- o .:? "idleTime"       .!= 30
    return RedisConfig{..}

defaultRedisConfig :: RedisConfig
defaultRedisConfig = RedisConfig
  { redisHost           = "127.0.0.1"
  , redisPort           = 6379
  , redisAuth           = ""
  , redisDB             = 0
  , redisMaxConnections = 50
  , redisMaxIdleTime    = 30
  , redisEnable         = False
  }

genRedisConnection :: RedisConfig -> IO (Maybe Connection)
genRedisConnection conf =
  if enable then do
      conn <- connect $ defaultConnectInfo
        { connectHost           = h
        , connectPort           = PortNumber p
        , connectAuth           = auth
        , connectDatabase       = db
        , connectMaxConnections = maxConnections
        , connectMaxIdleTime    = maxIdleTime
        }

      return (Just conn)
  else return Nothing

  where db             = redisDB             conf
        h              = redisHost           conf
        p              = fromIntegral $ redisPort conf
        enable         = redisEnable         conf
        maxConnections = redisMaxConnections conf
        maxIdleTime    = redisMaxIdleTime    conf
        auth           = if not (null (redisAuth conf)) then
                            Just $ fromString (redisAuth conf) else Nothing
