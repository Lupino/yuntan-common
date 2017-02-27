{-# LANGUAGE OverloadedStrings #-}

module Dispatch.Utils.JSON
  (
    replace
  , unionValue
  , differenceValue
  ) where

import           Data.Aeson          (Value (..))
import           Data.Text           (Text)

import           Data.HashMap.Strict (delete, difference, insert, lookupDefault,
                                      union)

replace :: Text -> Text -> Value -> Value
replace okey nkey (Object v) = Object . delete okey $ insert nkey ov v
  where ov = lookupDefault Null okey v

replace _ _ v = v

unionValue :: Value -> Value -> Value
unionValue (Object a) (Object b) = Object $ union a b
unionValue (Object a) _          = Object a
unionValue _ (Object b)          = Object b
unionValue _ _                   = Null

differenceValue :: Value -> Value -> Value
differenceValue (Object a) (Object b) = Object $ difference a b
differenceValue (Object a) _          = Object a
differenceValue _ _                   = Null
