module Database.PSQL.Types
  (
  ) where


import           Data.ByteString.Builder            (toLazyByteString)
import           Data.Hashable                      (Hashable (..))
import           Database.PostgreSQL.Simple.ToField (Action (..))


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
