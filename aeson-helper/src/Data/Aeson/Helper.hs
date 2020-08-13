{-# LANGUAGE OverloadedStrings #-}

module Data.Aeson.Helper
  ( replace
  , union
  , difference
  , pick
  ) where

import           Data.Aeson          (Value (..))
import           Data.Maybe          (catMaybes)
import           Data.Text           (Text)
import qualified Data.Text           as T (isPrefixOf, stripPrefix)
import qualified Data.Vector         as V (map)

import           Data.HashMap.Strict (delete, insert, lookupDefault,
                                      mapMaybeWithKey)
import qualified Data.HashMap.Strict as HM (difference, union)

replace :: Text -> Text -> Value -> Value
replace okey nkey (Object v) = Object . insert nkey ov $ delete okey v
  where ov = lookupDefault Null okey v

replace _ _ v = v

union :: Value -> Value -> Value
union (Object a) (Object b) = Object $ HM.union a b
union (Object a) _          = Object a
union _ (Object b)          = Object b
union _ _                   = Null

difference :: Value -> Value -> Value
difference (Object a) (Object b) = Object $ HM.difference a b
difference (Object a) _          = Object a
difference _ _                   = Null

-- key1.key2.key3
pick :: [Text] -> Value -> Value
pick [] v          = v
pick ks (Object a) = Object $ mapMaybeWithKey (doMapMaybeWithKey ks) a
pick ks (Array a)  = Array $ V.map (pick ks) a
pick _ _           = Null

doMapMaybeWithKey :: [Text] -> Text -> Value -> Maybe Value
doMapMaybeWithKey ks k v = go ks
  where go :: [Text] -> Maybe Value
        go [] = Nothing
        go (x:xs)
          | k == x = Just v
          | (k <> "." ) `T.isPrefixOf` x = Just $ pick (catMaybes $ nextKeys ks k) v
          | otherwise = go xs

nextKeys :: [Text] -> Text -> [Maybe Text]
nextKeys [] _     = []
nextKeys (x:xs) k = T.stripPrefix (k <> ".") x : nextKeys xs k
