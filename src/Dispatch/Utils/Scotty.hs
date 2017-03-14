{-# LANGUAGE OverloadedStrings #-}

module Dispatch.Utils.Scotty
  (
    maybeNotFound
  , eitherNotFound
  , eitherBadRequest
  , err
  , ok
  , errNotFound
  , errBadRequest
  , okListResult
  ) where

import           Dispatch.Types.ListResult (ListResult, fromListResult)
import qualified Dispatch.Types.Result     as R (ErrResult, err, fromOkResult,
                                                 ok)
import           Network.HTTP.Types        (Status, status400, status404)
import           Web.Scotty.Trans          (ActionT, ScottyError, json, status)

import           Data.Aeson                (ToJSON)
import           Data.Text                 (Text)

maybeNotFound :: (ToJSON a, ScottyError e, Monad m) => String -> Maybe a -> ActionT e m ()
maybeNotFound _ (Just a)  = json a
maybeNotFound obj Nothing = err status404 $ obj ++ " not found."

eitherBadRequest :: (ToJSON a, ScottyError e, Monad m) => Either R.ErrResult a -> ActionT e m ()
eitherBadRequest (Right a) = json a
eitherBadRequest (Left e)  = status status400 >> json e

eitherNotFound :: (ToJSON a, ScottyError e, Monad m) => Either R.ErrResult a -> ActionT e m ()
eitherNotFound (Right a) = json a
eitherNotFound (Left e)  = status status404 >> json e

err :: (ScottyError e, Monad m) => Status -> String -> ActionT e m ()
err st msg = status st >> json (R.err msg)

ok :: (ToJSON a, ScottyError e, Monad m) => Text -> a -> ActionT e m ()
ok key = json . R.fromOkResult key . R.ok

errNotFound :: (ScottyError e, Monad m) => String -> ActionT e m ()
errNotFound = err status404

errBadRequest :: (ScottyError e, Monad m) => String -> ActionT e m ()
errBadRequest = err status400

okListResult :: (ToJSON a, ScottyError e, Monad m) => Text -> ListResult a -> ActionT e m ()
okListResult key = json . fromListResult key
