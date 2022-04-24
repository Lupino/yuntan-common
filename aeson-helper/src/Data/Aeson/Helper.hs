{-# LANGUAGE OverloadedStrings #-}

module Data.Aeson.Helper
  ( replace
  , union
  , difference
  , pick
  ) where

import           Data.Aeson        (Value (..))
import           Data.Aeson.Key    (Key, toText)
import qualified Data.Aeson.KeyMap as KeyMap (delete, difference, insert,
                                              lookup, mapMaybeWithKey, union)
import           Data.Maybe        (catMaybes, fromMaybe)
import           Data.Text         (Text)
import qualified Data.Text         as T (isPrefixOf, stripPrefix)
import qualified Data.Vector       as V (map)

-- | Replace JSON key to a new key
--
-- >>> replace "okey" "nkey" (object [ "okey" := "value" ])
-- Object (fromList [("nkey",String "value")])
--
-- >>> replace "okey" "nkey" (String "value")
-- String "value"
replace :: Key -> Key -> Value -> Value
replace okey nkey (Object v) = Object . KeyMap.insert nkey ov $ KeyMap.delete okey v
  where ov = fromMaybe Null $ KeyMap.lookup okey v

replace _ _ v = v

-- | Union two JSON
--
-- >>> union (object ["key1" .= "value1"]) (object ["key2" .= "value2"])
-- Object (fromList [("key2",String "value2"),("key1",String "value1")])
--
-- >>> union (object ["key1" .= "value1"]) (object ["key1" .= "value2"])
-- Object (fromList [("key1",String "value1")])
--
-- >>> union Null (object ["key2" .= "value2"])
-- Object (fromList [("key2",String "value2")])
--
-- >>> union (object ["key1" .= "value1"]) Null
-- Object (fromList [("key1",String "value1")])
union :: Value -> Value -> Value
union (Object a) (Object b) = Object $ KeyMap.union a b
union (Object a) _          = Object a
union _ (Object b)          = Object b
union _ _                   = Null

-- | Difference two JSON
--
-- >>> difference  (object ["key1" .= "value1", "key2" .= "value2"]) (object ["key1" .= Null])
-- Object (fromList [("key2",String "value2")])
difference :: Value -> Value -> Value
difference (Object a) (Object b) = Object $ KeyMap.difference a b
difference (Object a) _          = Object a
difference _ _                   = Null

-- | Pick a value from JSON
--
-- >>> pick ["key1"] $ object ["key1" .= "value1", "key2" .= "value2", "key3" .= "value3"]
-- Object (fromList [("key1",String "value1")])
--
-- >>> pick ["key1", "key2"] $ object ["key1" .= "value1", "key2" .= "value2", "key3" .= "value3"]
-- Object (fromList [("key2",String "value2"),("key1",String "value1")])
--
-- >>> pick ["key3.key4"] $ object ["key1" .= "value1", "key2" .= "value2", "key3" .= object ["key4" .= "value4"]]
-- Object (fromList [("key3",Object (fromList [("key4",String "value4")]))])
pick :: [Text] -> Value -> Value
pick [] v          = v
pick ks (Object a) = Object $ KeyMap.mapMaybeWithKey (doMapMaybeWithKey ks) a
pick ks (Array a)  = Array $ V.map (pick ks) a
pick _ _           = Null

doMapMaybeWithKey :: [Text] -> Key -> Value -> Maybe Value
doMapMaybeWithKey ks key v = go ks
  where go :: [Text] -> Maybe Value
        go [] = Nothing
        go (x:xs)
          | k == x = Just v
          | (k <> "." ) `T.isPrefixOf` x = Just $ pick (catMaybes $ nextKeys ks k) v
          | otherwise = go xs

        k = toText key

nextKeys :: [Text] -> Text -> [Maybe Text]
nextKeys [] _     = []
nextKeys (x:xs) k = T.stripPrefix (k <> ".") x : nextKeys xs k
