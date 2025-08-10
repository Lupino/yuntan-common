{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Database.PSQL.Types.SimpleEnv
  ( SimpleEnv
  , simpleEnv
  ) where

import           Database.PSQL.Class.HasOtherEnv (HasOtherEnv (..))
import           Database.PSQL.Class.HasPSQL     (HasPSQL (..), PSQLPool)
import           Database.PSQL.Types.TablePrefix (TablePrefix)

data SimpleEnv u = SimpleEnv
    { pc :: PSQLPool
    , pf :: TablePrefix
    , pu :: u
    }

instance HasPSQL (SimpleEnv u) where
  psqlPool = pc
  tablePrefix = pf

instance HasOtherEnv u (SimpleEnv u) where
  otherEnv = pu

simpleEnv :: PSQLPool -> TablePrefix -> u -> SimpleEnv u
simpleEnv pool prefix env0 = SimpleEnv{pc=pool, pf = prefix, pu = env0}

