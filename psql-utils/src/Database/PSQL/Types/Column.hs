module Database.PSQL.Types.Column
  ( Column (..)

  , Columns
  , columnsToString
  ) where


import           Data.List   (intercalate)
import           Data.String (IsString (..))

newtype Column = Column { unColumn :: String }
  deriving (Show)

instance IsString Column where
  fromString = Column

type Columns = [Column]

columnsToString :: Columns -> String
columnsToString = intercalate ", " . map unColumn
