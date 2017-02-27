{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Dispatch.Types.ListResult
  (
    From
  , Size
  , Total
  , ListResult (..)
  , emptyListResult

  , toListResult
  , fromListResult
  ) where

import           Data.Aeson          (FromJSON (..), Result (..), ToJSON (..),
                                      Value, fromJSON, object, withObject, (.:),
                                      (.=))
import           Data.Int            (Int64)
import           Data.Text           (Text)

import           Dispatch.Utils.JSON (replace)

type From        = Int64
type Size        = Int64
type Total       = Int64

data ListResult a = ListResult { getFrom   :: From
                               , getSize   :: Size
                               , getTotal  :: Total
                               , getResult :: [a]
                               }
  deriving (Show)


instance FromJSON a => FromJSON (ListResult a) where
  parseJSON = withObject "ListResult" $ \o -> do
    getFrom   <- o .: "from"
    getSize   <- o .: "size"
    getTotal  <- o .: "total"
    getResult <- o .: "result"
    return ListResult{..}

instance ToJSON a => ToJSON (ListResult a) where
  toJSON ListResult{..} = object [ "from"   .= getFrom
                                 , "size"   .= getSize
                                 , "total"  .= getTotal
                                 , "result" .= getResult
                                 ]

emptyListResult :: ListResult a
emptyListResult = ListResult { getFrom = 0
                             , getSize = 10
                             , getTotal = 0
                             , getResult = []
                             }

toListResult :: FromJSON a => Text -> Value -> Maybe (ListResult a)
toListResult okey v = do
  case fromJSON (replace okey "result" v) of
    Success v' -> Just v'
    _          -> Nothing

fromListResult :: ToJSON a => Text -> ListResult a -> Value
fromListResult key ret = replace "result" key $ toJSON ret

