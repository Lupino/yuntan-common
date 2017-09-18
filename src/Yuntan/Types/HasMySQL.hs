module Yuntan.Types.HasMySQL
  (
    TablePrefix
  , HasMySQL
  , mysqlPool
  , tablePrefix
  , SimpleEnv
  , simpleEnv
  ) where


import           Data.Pool             (Pool)
import           Database.MySQL.Simple (Connection)

type TablePrefix = String

class HasMySQL u where
  mysqlPool :: u -> Pool Connection
  tablePrefix :: u -> TablePrefix

data SimpleEnv = SimpleEnv { pc :: Pool Connection
                           , pf :: String
                           }

instance HasMySQL SimpleEnv where
  mysqlPool = pc
  tablePrefix = pf

simpleEnv :: Pool Connection -> TablePrefix -> SimpleEnv
simpleEnv pool prefix = SimpleEnv{pc=pool, pf = prefix}
