{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.Scotty.Utils
  ( maybeNotFound
  , eitherNotFound
  , eitherBadRequest
  , err
  , ok
  , errNotFound
  , errBadRequest
  , okListResult
  , safeQueryParam
  , safeFormParam
  ) where


import           Control.Exception       (SomeException)
import           Control.Monad.IO.Class  (MonadIO)
import           Control.Monad.IO.Unlift (MonadUnliftIO)
import           Data.Aeson              (ToJSON)
import           Data.Aeson.Key          (Key)
import qualified Data.Aeson.Result       as R (Err, List, err, fromList, fromOk,
                                               ok)
import qualified Data.Text.Lazy          as LT (Text)
import           Network.HTTP.Types      (Status, status400, status404)
import           Web.Scotty.Trans        (ActionT, Parsable, formParam, json,
                                          queryParam, rescue, status)

maybeNotFound :: (ToJSON a, MonadIO m) => String -> Maybe a -> ActionT m ()
maybeNotFound _ (Just a)  = json a
maybeNotFound obj Nothing = err status404 $ obj ++ " not found."

eitherBadRequest :: (ToJSON a, MonadIO m) => Either R.Err a -> ActionT m ()
eitherBadRequest (Right a) = json a
eitherBadRequest (Left e)  = status status400 >> json e

eitherNotFound :: (ToJSON a, MonadIO m) => Either R.Err a -> ActionT m ()
eitherNotFound (Right a) = json a
eitherNotFound (Left e)  = status status404 >> json e

err :: MonadIO m => Status -> String -> ActionT m ()
err st msg = status st >> json (R.err msg)

ok :: (ToJSON a, MonadIO m) => Key -> a -> ActionT m ()
ok key = json . R.fromOk key . R.ok

errNotFound :: MonadIO m => String -> ActionT m ()
errNotFound = err status404

errBadRequest :: MonadIO m => String -> ActionT m ()
errBadRequest = err status400

okListResult :: (ToJSON a, MonadIO m) => Key -> R.List a -> ActionT m ()
okListResult key = json . R.fromList key

safeQueryParam ::(Parsable a, MonadUnliftIO m) => LT.Text -> a -> ActionT m a
safeQueryParam key def = queryParam key `rescue` (\(_ :: SomeException) -> return def)

safeFormParam ::(Parsable a, MonadUnliftIO m) => LT.Text -> a -> ActionT m a
safeFormParam key def = formParam key `rescue` (\(_ :: SomeException) -> return def)
