{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Database.PSQL.Types.TableName
  ( TableName
  , as
  , join
  , leftJoin
  , getTableName

  , IndexName
  , getIndexName
  ) where

import           Data.Hashable                   (Hashable (..))
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
tn `as` asName = TableNameAs tn asName

join :: TableName -> TableName -> TableName
name1 `join` name2 = TableNameJoin name1 name2

leftJoin :: TableName -> TableName -> String -> TableName
leftJoin = TableNameLeftJoin

getTableName :: TablePrefix -> TableName -> String
getTableName prefix (TableName name) =
  concat ["\"", getPrefix prefix, name, "\"" ]
getTableName prefix (TableNameAs name asName) =
  concat [ getTableName prefix name, " AS ", asName ]
getTableName prefix (TableNameJoin name1 name2) =
  concat [ getTableName prefix name1, ", ", getTableName prefix name2 ]

getTableName prefix (TableNameLeftJoin name1 name2 on) =
  concat [ getTableName prefix name1, " LEFT JOIN ", getTableName prefix name2, " ON ", on ]

newtype IndexName = IndexName String
  deriving (Show)

instance IsString IndexName where
  fromString = IndexName

getIndexName :: TablePrefix -> TableName -> IndexName -> String
getIndexName prefix (TableName tn) (IndexName name) =
  concat [ "\"", getPrefix prefix, tn , "_", name, "\"" ]
getIndexName _ _ _ = error "no implement"
