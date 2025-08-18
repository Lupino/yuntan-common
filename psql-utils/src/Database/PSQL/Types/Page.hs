{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}

module Database.PSQL.Types.Page
  ( Page (..)
  , pageAll
  , pageNone

  , page
  , pageAsc
  , pageDesc

  , pageOne
  ) where

import           Data.Hashable               (Hashable (..))
import           Database.PSQL.Types.From    (From)
import           Database.PSQL.Types.OrderBy (OrderBy, asc, desc, orderNone)
import           Database.PSQL.Types.Size    (Size)
import           GHC.Generics                (Generic)

data Page = Page
  { pageSize  :: Size
  , pageFrom  :: From
  , pageOrder :: OrderBy
  }
  deriving (Generic, Eq)

instance Hashable Page

instance Show Page where
  show Page {..}  = concat [show pageOrder, show pageFrom, show pageSize]

pageAll :: OrderBy -> Page
pageAll pageOrder = Page {..}
  where pageSize = 0
        pageFrom = 0

pageNone :: Page
pageNone = pageAll orderNone

page :: From -> Size -> Page
page pageFrom pageSize = Page { pageOrder = orderNone, .. }

pageOne :: Page
pageOne = page 0 1

pageAsc :: From -> Size -> String -> Page
pageAsc pageFrom pageSize f = Page { pageOrder = asc f, .. }

pageDesc :: From -> Size -> String -> Page
pageDesc pageFrom pageSize f = Page { pageOrder = desc f, .. }
