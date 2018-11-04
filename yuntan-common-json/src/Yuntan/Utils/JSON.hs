{-# LANGUAGE OverloadedStrings #-}

module Yuntan.Utils.JSON
  (
    replace
  , unionValue
  , differenceValue
  , pickValue
  ) where

import           Data.Aeson          (Value (..))
import           Data.Maybe          (catMaybes)
import           Data.Text           (Text)
import qualified Data.Text           as T (isPrefixOf, stripPrefix)
import qualified Data.Vector         as V (map)

import           Data.HashMap.Strict (delete, difference, insert, lookupDefault,
                                      mapMaybeWithKey, union)

replace :: Text -> Text -> Value -> Value
replace okey nkey (Object v) = Object . insert nkey ov $ delete okey v
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

-- key1.key2.key3
pickValue :: [Text] -> Value -> Value
pickValue [] v          = v
pickValue ks (Object a) = Object $ mapMaybeWithKey (doMapMaybeWithKey ks) a
pickValue ks (Array a)  = Array $ V.map (pickValue ks) a
pickValue _ _           = Null

doMapMaybeWithKey :: [Text] -> Text -> Value -> Maybe Value
doMapMaybeWithKey ks k v = go ks
  where go :: [Text] -> Maybe Value
        go [] = Nothing
        go (x:xs)
          | (k <> "." ) `T.isPrefixOf` x = Just $ pickValue (catMaybes $ nextKeys ks k) v
          | otherwise = go xs

nextKeys :: [Text] -> Text -> [Maybe Text]
nextKeys [] _     = []
nextKeys (x:xs) k = T.stripPrefix (k <> ".") x : nextKeys xs k
