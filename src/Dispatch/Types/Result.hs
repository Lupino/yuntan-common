{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Dispatch.Types.Result
  (
    OkResult
  , ErrResult
  , err
  , ok
  ) where

import           Data.Aeson (FromJSON (..), ToJSON (..), object, withObject,
                             (.:), (.=))

data OkResult = OkResult { okMsg :: String }
  deriving (Show)

instance FromJSON OkResult where
  parseJSON = withObject "OkResult" $ \o -> do
    okMsg <- o .: "result"
    return OkResult{..}

instance ToJSON OkResult where
  toJSON OkResult{..} = object [ "result"   .= okMsg ]

data ErrResult = ErrResult { errMsg :: String }
  deriving (Show)

instance FromJSON ErrResult where
  parseJSON = withObject "ErrResult" $ \o -> do
    errMsg <- o .: "err"
    return ErrResult{..}

instance ToJSON ErrResult where
  toJSON ErrResult{..} = object [ "err"   .= errMsg ]

err :: String -> ErrResult
err = ErrResult


ok :: String -> OkResult
ok = OkResult
