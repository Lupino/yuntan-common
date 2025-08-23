{-# LANGUAGE DeriveGeneric #-}

module Database.PSQL.Types.Column
  ( Column (..)

  , Columns
  , columnsToString
  ) where


import           Data.Hashable (Hashable (..))
import           Data.List     (intercalate)
import           Data.String   (IsString (..))
import           GHC.Generics  (Generic)

newtype Column = Column { unColumn :: String }
  deriving (Generic, Eq, Ord, Show)

instance IsString Column where
  fromString = Column

instance Hashable Column

type Columns = [Column]

columnsToString :: Columns -> String
columnsToString = intercalate ", " . map unColumn
