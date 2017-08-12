{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Yuntan.Config.InfluxConfig
  (
    InfluxConfig (..)
  , genWriteParams
  , defaultInfluxConfig
  , InfluxHandle
  , newInfluxHandle
  , getInflux
  , updateInfluxHandle
  ) where

import           Data.Aeson              (FromJSON, parseJSON, withObject,
                                          (.!=), (.:), (.:?))

import           Data.Text               (Text)

import           Control.Lens            ((&), (.~))
import           Data.String             (fromString)
import           Database.InfluxDB.Types (Credentials (..), authentication,
                                          defaultServer, host, port)
import           Database.InfluxDB.Write (WriteParams, server, writeParams)

import           Data.IORef              (IORef, atomicWriteIORef, newIORef,
                                          readIORef)

data InfluxConfig = InfluxConfig { influxHost   :: Text
                                 , influxPort   :: Int
                                 , influxDBName :: String
                                 , influxAuth   :: Bool
                                 , influxUser   :: Text
                                 , influxPasswd :: Text
                                 , influxEnable :: Bool
                                 }
  deriving (Show)

instance FromJSON InfluxConfig where
  parseJSON = withObject "InfluxConfig" $ \o -> do
    influxDBName <- o .:  "db"
    influxHost   <- o .:? "host"   .!= "127.0.0.1"
    influxPort   <- o .:? "port"   .!= 8086
    influxAuth   <- o .:? "auth"   .!= False
    influxUser   <- o .:? "user"   .!= "user"
    influxPasswd <- o .:? "passwd" .!= "passwd"
    influxEnable <- o .:? "enable" .!= False
    return InfluxConfig{..}

defaultInfluxConfig :: InfluxConfig
defaultInfluxConfig = InfluxConfig { influxHost   = "127.0.0.1"
                                   , influxPort   = 8086
                                   , influxDBName = ""
                                   , influxAuth   = False
                                   , influxUser   = "user"
                                   , influxPasswd = "passwd"
                                   , influxEnable = False
                                   }

genWriteParams :: InfluxConfig -> Maybe WriteParams
genWriteParams conf | enable = Just $ writeParams db & server .~ s & authentication .~ auth
                    | otherwise = Nothing

  where db     = fromString $ influxDBName conf
        h      =  influxHost conf
        p      =  influxPort conf
        auth   = if influxAuth conf then Just (Credentials { _user = influxUser conf
                                                           , _password = influxPasswd conf
                                                           }
                                              )
                                    else Nothing
        enable = influxEnable conf
        s      = defaultServer & host .~ h & port .~ p

newtype InfluxHandle = InfluxHandle (IORef (Maybe WriteParams))

newInfluxHandle :: Maybe WriteParams -> IO InfluxHandle
newInfluxHandle params = InfluxHandle <$> newIORef params

getInflux :: InfluxHandle -> IO (Maybe WriteParams)
getInflux (InfluxHandle ref) = readIORef ref

updateInfluxHandle :: (Maybe WriteParams) -> InfluxHandle -> IO ()
updateInfluxHandle params (InfluxHandle ref) = atomicWriteIORef ref params
