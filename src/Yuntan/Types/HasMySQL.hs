module Yuntan.Types.HasMySQL
  (
    TablePrefix
  , HasMySQL
  , mysqlPool
  , tablePrefix
  ) where


import           Data.Pool             (Pool)
import           Database.MySQL.Simple (Connection)

type TablePrefix = String

class HasMySQL u where
  mysqlPool :: u -> Pool Connection
  tablePrefix :: u -> TablePrefix
