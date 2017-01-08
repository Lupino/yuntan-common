{-# LANGUAGE OverloadedStrings #-}

module Dispatch.Utils.Scotty
  (
    maybeNotFound
  , eitherNotFound
  , eitherBadRequest
  ) where

import           Dispatch.Types.Result (ErrResult, err)
import           Network.HTTP.Types    (status400, status404)
import           Web.Scotty.Trans      (ActionT, ScottyError, json, status)

import           Data.Aeson            (ToJSON)

maybeNotFound :: (ToJSON a, ScottyError e, Monad m) => String -> Maybe a -> ActionT e m ()
maybeNotFound _ (Just a) = json a
maybeNotFound obj Nothing = status status404
                            >> json (err $ obj ++ " not found.")

eitherBadRequest :: (ToJSON a, ScottyError e, Monad m) => Either ErrResult a -> ActionT e m ()
eitherBadRequest (Right a) = json a
eitherBadRequest (Left e)  = status status400 >> json e

eitherNotFound :: (ToJSON a, ScottyError e, Monad m) => Either ErrResult a -> ActionT e m ()
eitherNotFound (Right a) = json a
eitherNotFound (Left e)  = status status404 >> json e
