{-# LANGUAGE DeriveGeneric #-}

module Database.PSQL.Types.TableName
  ( TableName
  , as
  , join
  , leftJoin
  , getTableName
  , getDMLTableName
  , getDDLTableName

  , IndexName
  , getIndexName
  ) where

import           Data.Hashable                   (Hashable (..))
import           Data.Char                       (isSpace)
import           Data.String                     (IsString (..))
import           Database.PSQL.Types.TablePrefix (TablePrefix, getPrefix)
import           GHC.Generics                    (Generic)

data TableName =
  TableName String
  | TableNameAs TableName String
  | TableNameJoin TableName TableName
  | TableNameLeftJoin TableName TableName String
  deriving (Generic, Show, Eq, Ord)

instance Hashable TableName

instance IsString TableName where
  fromString = TableName

as :: TableName -> String -> TableName
tn `as` asName
  | all isSpace asName = error "as: empty alias"
  | hasOuterSpace asName = error "as: invalid alias"
  | hasQuote asName = error "as: quoted identifiers are not supported"
  | otherwise = TableNameAs tn asName

join :: TableName -> TableName -> TableName
name1 `join` name2 = TableNameJoin name1 name2

leftJoin :: TableName -> TableName -> String -> TableName
leftJoin = TableNameLeftJoin

getTableName :: TablePrefix -> TableName -> String
getTableName prefix (TableName name) =
  quoteQualifiedTableName prefix name
getTableName prefix (TableNameAs name asName) =
  concat [ renderAliasedName prefix name, " AS ", quoteAlias asName ]
getTableName prefix (TableNameJoin name1 name2) =
  concat [ getTableName prefix name1, ", ", getTableName prefix name2 ]

getTableName prefix (TableNameLeftJoin name1 name2 on) =
  concat [ getTableName prefix name1, " LEFT JOIN ", getTableName prefix name2, " ON ", on ]

getDMLTableName :: TablePrefix -> TableName -> String
getDMLTableName prefix (TableName name) = quoteQualifiedTableName prefix name
getDMLTableName prefix (TableNameAs baseName asName) =
  case baseName of
    TableName {} ->
      concat [ getDMLTableName prefix baseName, " AS ", quoteAlias asName ]
    _ ->
      error "getDMLTableName: aliased joined table names are not supported"
getDMLTableName _ TableNameJoin {}      = error "getDMLTableName: joined table names are not supported"
getDMLTableName _ TableNameLeftJoin {}  = error "getDMLTableName: joined table names are not supported"

getDDLTableName :: TablePrefix -> TableName -> String
getDDLTableName prefix (TableName name) = quoteQualifiedTableName prefix name
getDDLTableName _ TableNameAs {}        = error "getDDLTableName: aliased table names are not supported"
getDDLTableName _ TableNameJoin {}      = error "getDDLTableName: joined table names are not supported"
getDDLTableName _ TableNameLeftJoin {}  = error "getDDLTableName: joined table names are not supported"

renderAliasedName :: TablePrefix -> TableName -> String
renderAliasedName prefix tn@TableName {} = getTableName prefix tn
renderAliasedName prefix tn              = "(" ++ getTableName prefix tn ++ ")"

newtype IndexName = IndexName String
  deriving (Show)

instance IsString IndexName where
  fromString = IndexName

getIndexName :: TablePrefix -> TableName -> IndexName -> String
getIndexName prefix tn (IndexName name) =
  case () of
    _ | all isSpace name -> error "getIndexName: empty index name"
    _ ->
      case getBaseTableName tn of
        Just baseName ->
          case splitOnDot baseName of
            parts | any isBlankIdent parts || any hasOuterSpace parts ->
              error "getIndexName: invalid base table name"
            parts | any hasQuote parts ->
              error "getIndexName: quoted identifiers are not supported"
            _ ->
              case stripQualifier baseName of
                tableNamePart | isBlankIdent tableNamePart ->
                  error "getIndexName: invalid base table name"
                tableNamePart | hasOuterSpace tableNamePart || hasOuterSpace name ->
                  error "getIndexName: invalid index name"
                tableNamePart
                  | hasQuote tableNamePart || hasQuote name ->
                      error "getIndexName: quoted identifiers are not supported"
                  | otherwise ->
                      quoteIdent (getPrefix prefix ++ tableNamePart ++ "_" ++ name)
        Nothing ->
          error "getIndexName: joined table names are not supported"

quoteIdent :: String -> String
quoteIdent s = "\"" ++ concatMap escape s ++ "\""
  where
    escape '"' = "\"\""
    escape c   = [c]

quoteQualifiedTableName :: TablePrefix -> String -> String
quoteQualifiedTableName prefix name =
  case splitOnDot name of
    parts | any isBlankIdent parts -> error "getTableName: invalid table name"
    parts | any hasOuterSpace parts -> error "getTableName: invalid table name"
    parts | any hasQuote parts -> error "getTableName: quoted identifiers are not supported"
    [tableN] -> quoteIdent (getPrefix prefix ++ tableN)
    xs       -> unwordsJoinDot (map quoteIdent (init xs) ++ [quoteIdent (getPrefix prefix ++ last xs)])

splitOnDot :: String -> [String]
splitOnDot = go []
  where
    go acc [] = [reverse acc]
    go acc ('.':rest) = reverse acc : go [] rest
    go acc (c:rest) = go (c:acc) rest

unwordsJoinDot :: [String] -> String
unwordsJoinDot [] = ""
unwordsJoinDot [x] = x
unwordsJoinDot (x:xs) = x ++ "." ++ unwordsJoinDot xs

stripQualifier :: String -> String
stripQualifier = reverse . takeWhile (/= '.') . reverse

hasQuote :: String -> Bool
hasQuote = elem '"'

isBlankIdent :: String -> Bool
isBlankIdent s = null s || all isSpace s

hasOuterSpace :: String -> Bool
hasOuterSpace [] = False
hasOuterSpace [c] = isSpace c
hasOuterSpace s@(c:_)= isSpace c || isSpace (last s)

quoteAlias :: String -> String
quoteAlias aliasName
  | isBlankIdent aliasName = error "getTableName: invalid alias"
  | hasOuterSpace aliasName = error "getTableName: invalid alias"
  | hasQuote aliasName = error "getTableName: quoted identifiers are not supported"
  | otherwise = quoteIdent aliasName

getBaseTableName :: TableName -> Maybe String
getBaseTableName (TableName name)       = Just name
getBaseTableName (TableNameAs name _)   = getBaseTableName name
getBaseTableName (TableNameJoin _ _)    = Nothing
getBaseTableName (TableNameLeftJoin {}) = Nothing
