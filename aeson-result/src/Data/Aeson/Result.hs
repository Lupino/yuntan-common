{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Data.Aeson.Result
  ( Ok (..)
  , Err (..)
  , err
  , ok
  , fromOk
  , toOk
  , throwError

  , From
  , Size
  , Total
  , List (..)
  , emptyList
  , merge

  , toList
  , fromList
  ) where

import           Control.Exception (Exception, throwIO)
import           Data.Aeson        (FromJSON (..), Result (..), ToJSON (..),
                                    Value, fromJSON, object, withObject, (.:),
                                    (.=))
import           Data.Aeson.Helper (replace)
import           Data.Aeson.Key    (Key)
import           Data.Int          (Int64)

type From  = Int64
type Size  = Int64
type Total = Int64

-- | Make ok result look like    '{"data": "data value"}'
newtype Ok a = Ok { getValue :: a }
  deriving (Show)

instance (FromJSON a) => FromJSON (Ok a) where
  parseJSON = withObject "Ok" $ \o -> do
    getValue <- o .: "result"
    return Ok{..}

instance (ToJSON a) => ToJSON (Ok a) where
  toJSON Ok{..} = object [ "result" .= getValue ]

-- | Make error result look like    '{"err": "error message"}'
newtype Err = Err { errMsg :: String }
  deriving (Show, Eq, Ord)

instance Exception Err

-- | Throw error to IO
throwError :: Err -> IO a
throwError = throwIO

instance FromJSON Err where
  parseJSON = withObject "Err" $ \o -> do
    errMsg <- o .: "err"
    return Err{..}

instance ToJSON Err where
  toJSON Err{..} = object [ "err" .= errMsg ]

-- | Initial Err
err :: String -> Err
err = Err


-- | Initial Ok
ok :: a -> Ok a
ok = Ok

-- | Make a JSON result to Ok
toOk :: FromJSON a => Key -> Value -> Maybe (Ok a)
toOk okey v =
  case fromJSON (replace okey "result" v) of
    Success v' -> Just v'
    _          -> Nothing

-- | Make an Ok to JSON
fromOk :: ToJSON a => Key -> Ok a -> Value
fromOk key ret = replace "result" key $ toJSON ret

-- | Make list result look like '{"users": ["user1"], "total": 1, "from": 0, "size": 10}'
data List a = List
  { getFrom   :: From
  , getSize   :: Size
  , getTotal  :: Total
  , getResult :: [a]
  }
  deriving (Show)


instance FromJSON a => FromJSON (List a) where
  parseJSON = withObject "List" $ \o -> do
    getFrom   <- o .: "from"
    getSize   <- o .: "size"
    getTotal  <- o .: "total"
    getResult <- o .: "result"
    return List{..}

instance ToJSON a => ToJSON (List a) where
  toJSON List{..} = object
    [ "from"   .= getFrom
    , "size"   .= getSize
    , "total"  .= getTotal
    , "result" .= getResult
    ]

-- | Empty list
emptyList :: List a
emptyList = List
  { getFrom = 0
  , getSize = 10
  , getTotal = 0
  , getResult = []
  }

-- | Merge two list, from size and total, result is replace.
merge :: [a] -> List b -> List a
merge t List
  { getFrom = from
  , getSize = size
  , getTotal = total
  } = List
  { getFrom = from
  , getSize = size
  , getTotal = total
  , getResult = t
  }

-- | Make a JSON to List
toList :: FromJSON a => Key -> Value -> Maybe (List a)
toList okey v =
  case fromJSON (replace okey "result" v) of
    Success v' -> Just v'
    _          -> Nothing

-- | Make a List to JSON
fromList :: ToJSON a => Key -> List a -> Value
fromList key ret = replace "result" key $ toJSON ret
