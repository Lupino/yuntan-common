module Database.PSQL.Types
  ( TablePrefix

  , Connection

  , constraintPrimaryKey
  , Columns
  , Column (..)

  -- re-exports
  , FromRow (..)
  , FromField (..)
  , ToRow (..)
  , ToField (..)
  , Action (..)
  , field
  , Only (..)
  , SqlError (..)

  , module X
  ) where


import           Data.ByteString.Builder              (toLazyByteString)
import           Data.Hashable                        (Hashable (..))
import           Database.PostgreSQL.Simple           (Connection, Only (..),
                                                       SqlError (..))
import           Database.PostgreSQL.Simple.FromField (FromField (..))
import           Database.PostgreSQL.Simple.FromRow   (FromRow (..), field)
import           Database.PostgreSQL.Simple.ToField   (Action (..),
                                                       ToField (..))
import           Database.PostgreSQL.Simple.ToRow     (ToRow (..))
import           Database.PSQL.Class.HasOtherEnv      as X
import           Database.PSQL.Class.HasPSQL          as X
import           Database.PSQL.Exc                    as X
import           Database.PSQL.Gen                    (constraintPrimaryKey)
import           Database.PSQL.Select                 as X
import           Database.PSQL.Types.Column           (Column (..), Columns)
import           Database.PSQL.Types.From             as X
import           Database.PSQL.Types.GroupBy          as X
import           Database.PSQL.Types.OrderBy          as X
import           Database.PSQL.Types.Page             as X
import           Database.PSQL.Types.PSQL             as X
import           Database.PSQL.Types.SimpleEnv        as X
import           Database.PSQL.Types.Size             as X
import           Database.PSQL.Types.TableName        as X
import           Database.PSQL.Types.TablePrefix      (TablePrefix)
import           Database.PSQL.Util                   as X
import           Database.PSQL.Version                as X


instance Eq Action where
  Plain b1 == Plain b2                         = toLazyByteString b1 == toLazyByteString b2
  Escape bs1 == Escape bs2                     = bs1 == bs2
  EscapeByteA bs1 == EscapeByteA bs2           = bs1 == bs2
  EscapeIdentifier bs1 == EscapeIdentifier bs2 = bs1 == bs2
  Many actions1 == Many actions2               = actions1 == actions2
  _ == _                                       = False

instance Hashable Action where
  hashWithSalt s (Plain v)            = hashWithSalt s (0::Int, toLazyByteString v)
  hashWithSalt s (Escape v)           = hashWithSalt s (1::Int, v)
  hashWithSalt s (EscapeByteA v)      = hashWithSalt s (2::Int, v)
  hashWithSalt s (EscapeIdentifier v) = hashWithSalt s (3::Int, v)
  hashWithSalt s (Many v)             = hashWithSalt s (4::Int, v)
