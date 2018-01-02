{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Yuntan.Types.Result
  (
    OkResult (..)
  , ErrResult (..)
  , err
  , ok
  , fromOkResult
  , toOkResult
  ) where

import           Data.Aeson        (FromJSON (..), Result (..), ToJSON (..),
                                    Value, fromJSON, object, withObject, (.:),
                                    (.=))
import           Data.Text         (Text)
import           Yuntan.Utils.JSON (replace)

newtype OkResult a = OkResult { getValue :: a }
  deriving (Show)

instance (FromJSON a) => FromJSON (OkResult a) where
  parseJSON = withObject "OkResult" $ \o -> do
    getValue <- o .: "result"
    return OkResult{..}

instance (ToJSON a) => ToJSON (OkResult a) where
  toJSON OkResult{..} = object [ "result" .= getValue ]

newtype ErrResult = ErrResult { errMsg :: String }
  deriving (Show)

instance FromJSON ErrResult where
  parseJSON = withObject "ErrResult" $ \o -> do
    errMsg <- o .: "err"
    return ErrResult{..}

instance ToJSON ErrResult where
  toJSON ErrResult{..} = object [ "err" .= errMsg ]

err :: String -> ErrResult
err = ErrResult


ok :: a -> OkResult a
ok = OkResult

toOkResult :: FromJSON a => Text -> Value -> Maybe (OkResult a)
toOkResult okey v =
  case fromJSON (replace okey "result" v) of
    Success v' -> Just v'
    _          -> Nothing

fromOkResult :: ToJSON a => Text -> OkResult a -> Value
fromOkResult key ret = replace "result" key $ toJSON ret
