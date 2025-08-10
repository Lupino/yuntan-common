module Database.PSQL.Class.HasPSQL
  ( HasPSQL (..)

  , PSQLPool
  ) where

import           Data.Pool                       (Pool)
import           Database.PostgreSQL.Simple      (Connection)
import           Database.PSQL.Types.TablePrefix (TablePrefix)

type PSQLPool = Pool Connection

class HasPSQL u where
  psqlPool    :: u -> PSQLPool
  tablePrefix :: u -> TablePrefix
