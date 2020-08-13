{-# LANGUAGE OverloadedStrings #-}

module Yuntan.Utils.Scotty
  (
    maybeNotFound
  , eitherNotFound
  , eitherBadRequest
  , err
  , ok
  , errNotFound
  , errBadRequest
  , okListResult
  , safeParam
  ) where

import           Network.HTTP.Types (Status, status400, status404)
import           Web.Scotty.Trans   (ActionT, Parsable, ScottyError, json,
                                     param, rescue, status)

import qualified Data.Aeson.Result  as R (Err, List, err, fromList, fromOk, ok)

import           Data.Aeson         (ToJSON)
import           Data.Text          (Text)
import qualified Data.Text.Lazy     as LT (Text)

maybeNotFound :: (ToJSON a, ScottyError e, Monad m) => String -> Maybe a -> ActionT e m ()
maybeNotFound _ (Just a)  = json a
maybeNotFound obj Nothing = err status404 $ obj ++ " not found."

eitherBadRequest :: (ToJSON a, ScottyError e, Monad m) => Either R.Err a -> ActionT e m ()
eitherBadRequest (Right a) = json a
eitherBadRequest (Left e)  = status status400 >> json e

eitherNotFound :: (ToJSON a, ScottyError e, Monad m) => Either R.Err a -> ActionT e m ()
eitherNotFound (Right a) = json a
eitherNotFound (Left e)  = status status404 >> json e

err :: (ScottyError e, Monad m) => Status -> String -> ActionT e m ()
err st msg = status st >> json (R.err msg)

ok :: (ToJSON a, ScottyError e, Monad m) => Text -> a -> ActionT e m ()
ok key = json . R.fromOk key . R.ok

errNotFound :: (ScottyError e, Monad m) => String -> ActionT e m ()
errNotFound = err status404

errBadRequest :: (ScottyError e, Monad m) => String -> ActionT e m ()
errBadRequest = err status400

okListResult :: (ToJSON a, ScottyError e, Monad m) => Text -> R.List a -> ActionT e m ()
okListResult key = json . R.fromList key

safeParam ::(Parsable a, ScottyError e, Monad m) => LT.Text -> a -> ActionT e m a
safeParam key def = param key `rescue` (\_ -> return def)
