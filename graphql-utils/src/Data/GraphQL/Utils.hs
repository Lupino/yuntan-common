{-# LANGUAGE OverloadedStrings #-}

module Data.GraphQL.Utils
  ( get
  , getInt
  , getFloat
  , getText
  , getEnum
  , getBool
  , getObject
  , getList
  , value
  , value'
  , pick
  ) where

import           Control.Applicative   (Alternative (..))
import qualified Data.Aeson            as A (Value (..))
import qualified Data.Aeson.Helper     as J (pick)
import           Data.GraphQL.AST      (Name)
import           Data.GraphQL.AST.Core (ObjectField)
import           Data.GraphQL.Schema   (Argument (..), Resolver, Value (..),
                                        array, object, scalar, scalarA)
import qualified Data.HashMap.Strict   as HM (toList)
import           Data.Maybe            (fromMaybe, mapMaybe)
import           Data.Text             (Text)
import qualified Data.Vector           as V (Vector, head, null, toList)

get :: Name -> [Argument] -> Maybe Value
get _ [] = Nothing
get k (Argument n v:xs) | k == n = Just v
                        | otherwise = get k xs

getInt :: Num a => Name -> [Argument] -> Maybe a
getInt n argv =
  case get n argv of
    (Just (ValueInt v)) -> Just $ fromIntegral v
    _                   -> Nothing

getFloat :: Name -> [Argument] -> Maybe Double
getFloat n argv =
  case get n argv of
    (Just (ValueFloat v)) -> Just v
    _                     -> Nothing

getBool :: Name -> [Argument] -> Maybe Bool
getBool n argv =
  case get n argv of
    (Just (ValueBoolean v)) -> Just v
    _                       -> Nothing

getText :: Name -> [Argument] -> Maybe Text
getText n argv =
  case get n argv of
    (Just (ValueString v)) -> Just v
    _                      -> Nothing

getEnum :: Name -> [Argument] -> Maybe Name
getEnum n argv =
  case get n argv of
    (Just (ValueEnum v)) -> Just v
    _                    -> Nothing

getObject :: Name -> [Argument] -> Maybe [ObjectField]
getObject n argv =
  case get n argv of
    (Just (ValueObject v)) -> Just v
    _                      -> Nothing

getList :: Name -> [Argument] -> Maybe [Value]
getList n argv =
  case get n argv of
    (Just (ValueList v)) -> Just v
    _                    -> Nothing

value :: Alternative f => Name -> A.Value -> Resolver f
value k (A.Object v) = object k . listToResolver $ HM.toList v
value k (A.Array v)  = if isO v then array k (map value' $ V.toList v)
                                else scalar k v
value k v            = scalar k v

isOv :: A.Value -> Bool
isOv (A.Object _) = True
isOv _            = False

isO :: V.Vector A.Value -> Bool
isO v | V.null v  = False
      | otherwise = isOv $ V.head v

value' :: Alternative f => A.Value -> [Resolver f]
value' (A.Object v) = listToResolver $ HM.toList v
value' _            = []

listToResolver :: Alternative f => [(Text, A.Value)] -> [Resolver f]
listToResolver []          = []
listToResolver ((k, v):xs) = value k v : listToResolver xs

pick :: Alternative f => Name -> A.Value -> Resolver f
pick n v = scalarA n $ \args -> pure $ J.pick (keys args) v
  where keys :: [Argument] -> [Text]
        keys args = mapMaybe getText' (fromMaybe [] $ getList "keys" args)

        getText' :: Value -> Maybe Text
        getText' (ValueString k) = Just k
        getText' _               = Nothing
