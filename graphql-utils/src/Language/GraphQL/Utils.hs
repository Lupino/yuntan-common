{-# LANGUAGE OverloadedStrings #-}

module Language.GraphQL.Utils
  ( get
  , getInt
  , getFloat
  , getText
  , getEnum
  , getBool
  , getObject
  , getList
  , mapValue
  , mapType
  -- , value
  -- , value'
  -- , pick
  ) where

import           Control.Monad.Trans.Reader (asks)
import qualified Data.Aeson                 as A (Value (..))
import qualified Data.HashMap.Strict        as HM (map, mapWithKey, (!))
import           Data.Scientific            (floatingOrInteger)
import           Data.Text                  (Text)
import qualified Data.Vector                as V (Vector, head, null, toList)
import           Language.GraphQL.AST       (Argument (..), Name, ObjectField)
import qualified Language.GraphQL.AST       as AST (Value (..))
import           Language.GraphQL.Type      (Field (..), Resolve, Resolver (..),
                                             ScalarType (..), boolean, float,
                                             int, string, values)
import qualified Language.GraphQL.Type      as Type (Value (..))
import qualified Language.GraphQL.Type.Out  as Out

get :: Name -> [Argument] -> Maybe AST.Value
get _ [] = Nothing
get k (Argument n v:xs) | k == n = Just v
                        | otherwise = get k xs

getInt :: Num a => Name -> [Argument] -> Maybe a
getInt n argv =
  case get n argv of
    (Just (AST.Int v)) -> Just $ fromIntegral v
    _                  -> Nothing

getFloat :: Name -> [Argument] -> Maybe Double
getFloat n argv =
  case get n argv of
    (Just (AST.Float v)) -> Just v
    _                    -> Nothing

getBool :: Name -> [Argument] -> Maybe Bool
getBool n argv =
  case get n argv of
    (Just (AST.Boolean v)) -> Just v
    _                      -> Nothing

getText :: Name -> [Argument] -> Maybe Text
getText n argv =
  case get n argv of
    (Just (AST.String v)) -> Just v
    _                     -> Nothing

getEnum :: Name -> [Argument] -> Maybe Name
getEnum n argv =
  case get n argv of
    (Just (AST.Enum v)) -> Just v
    _                   -> Nothing

getObject :: Name -> [Argument] -> Maybe [ObjectField AST.Value]
getObject n argv =
  case get n argv of
    (Just (AST.Object v)) -> Just v
    _                     -> Nothing

getList :: Name -> [Argument] -> Maybe [AST.Value]
getList n argv =
  case get n argv of
    (Just (AST.List v)) -> Just v
    _                   -> Nothing

mapValue :: A.Value -> Type.Value
mapValue (A.String v) = Type.String v
mapValue (A.Number v) =
  case floatingOrInteger v of
    Left vv  -> Type.Float vv
    Right vv -> Type.Int vv
mapValue (A.Bool v) = Type.Boolean v
mapValue A.Null = Type.Null
mapValue (A.Array v) = Type.List $ map mapValue $ V.toList v
mapValue (A.Object v) = Type.Object $ HM.map mapValue v

idField :: Monad m => Text -> Resolve m
idField f = do
    v <- asks values
    let (Type.Object v') = v
    pure $ v' HM.! f

mapType :: Monad m => A.Value -> Out.Type m
mapType (A.String _) = Out.NamedScalarType string
mapType (A.Number v) =
  case floatingOrInteger v of
    Left _  -> Out.NamedScalarType float
    Right _ -> Out.NamedScalarType int
mapType (A.Bool _) = Out.NamedScalarType boolean
mapType A.Null = Out.NamedScalarType (ScalarType "Null" Nothing)
mapType (A.Array v) | isO v = Out.ListType $ mapType A.Null
                    | otherwise = Out.ListType $ mapType $ V.head v
mapType (A.Object v) =
  Out.NamedObjectType $ Out.ObjectType "Value" Nothing [] $ HM.mapWithKey mapFunc v

  where mapFunc :: Monad m => Text -> A.Value -> Resolver m
        mapFunc k vv = ValueResolver (Field Nothing (mapType vv) mempty) (idField k)

--   Out.NamedObjectType
--   $ Out.ObjectType "Value" Nothing []
--   $ HM.map mapType v

-- value :: Monad m => A.Value -> Resolver m
-- -- value k (A.Object v) = object k . listToResolver $ HM.toList v
-- -- value k (A.Array v)  = if isO v then array k (map value' $ V.toList v)
-- --                                 else scalar k v
-- value (A.String v) = ValueResolver stringField (stringValue v)
-- value (A.Number v) =
--   case floatingOrInteger v of
--     Left vv  -> ValueResolver floatField (floatValue vv)
--     Right vv -> ValueResolver intField (intValue vv)
-- value A.Null = ValueResolver nullField nullValue
-- value (A.Bool v) = ValueResolver booleanField (booleanValue v)

isOv :: A.Value -> Bool
isOv (A.Object _) = True
isOv _            = False

isO :: V.Vector A.Value -> Bool
isO v | V.null v  = False
      | otherwise = isOv $ V.head v

-- value' :: Alternative f => A.Value -> [Resolver f]
-- value' (A.Object v) = listToResolver $ HM.toList v
-- value' _            = []
--
-- listToResolver :: Alternative f => [(Text, A.Value)] -> [Resolver f]
-- listToResolver []          = []
-- listToResolver ((k, v):xs) = value k v : listToResolver xs

-- pick :: Monad m => A.Value -> Resolve m
-- pick v = do
--   args <- keys <$> asks arguments
--   -- scalarA n $ \args -> pure $ J.pick (keys args) v
--   where keys :: [Argument] -> [Text]
--         keys args = mapMaybe getText' (fromMaybe [] $ getList "keys" args)
--
--         getText' :: AST.Value -> Maybe Text
--         getText' (AST.String k) = Just k
--         getText' _              = Nothing
