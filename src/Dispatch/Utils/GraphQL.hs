{-# LANGUAGE OverloadedStrings #-}

module Dispatch.Utils.GraphQL
  (
    getValue
  , value
  , value'
  ) where

import           Control.Applicative (Alternative (..))

import qualified Data.Aeson          as A (Value (..))
import           Data.GraphQL.AST    (Name)
import           Data.GraphQL.Schema (Argument (..), Resolver, Value, array,
                                      object, scalar)
import qualified Data.HashMap.Strict as HM (toList)
import           Data.Text           (Text)
import qualified Data.Vector         as V (Vector, head, null, toList)

import           Haxl.Core           (GenHaxl, throw)
import           Haxl.Prelude        (NotFound (..), catchAny)


instance Alternative (GenHaxl u) where
  a <|> b = catchAny a b
  empty = throw $ NotFound "mzero"

getValue :: Name -> [Argument] -> Maybe Value
getValue _ [] = Nothing
getValue k (Argument n v:xs) | k == n = Just v
                             | otherwise = getValue k xs

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