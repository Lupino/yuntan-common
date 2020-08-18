{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Network.Wreq.Helper
  ( responseValue
  , responseMaybe
  , responseEither
  , responseEither'
  , responseEitherJSON
  , responseJSON
  , responseOk
  , responseOk_
  , responseList
  , responseList_
  , tryResponse
  , eitherToError
  ) where

import           Control.Exception     (try)
import           Control.Lens          ((^.), (^?))
import           Data.Aeson            (FromJSON (..), decode)
import           Data.Aeson.Result     (Err, List, Ok, err, throwError, toList,
                                        toOk)
import qualified Data.ByteString.Char8 as B (unpack)
import qualified Data.ByteString.Lazy  as LB (ByteString, fromStrict)
import           Data.Text             (Text)
import           Network.HTTP.Client   (HttpException (..),
                                        HttpExceptionContent (..))
import           Network.Wreq          (Response, asJSON, responseBody)

eitherToError :: IO (Either Err a) -> IO a
eitherToError io  = do
  r <- io
  case r of
    Left e  -> throwError e
    Right v -> pure v

responseValue :: IO (Response a) -> IO a
responseValue req = do
  r <- req
  return $ r ^. responseBody

responseMaybe :: IO (Response a) -> IO (Maybe a)
responseMaybe req = do
  e <- try req
  case e of
    Left (_ :: HttpException) -> return Nothing
    Right r                   -> return $ r ^? responseBody

tryResponse :: IO (Response a) -> IO (Either Err (Response a))
tryResponse req = do
  e <- try req
  case e of
    Left (HttpExceptionRequest _ content) ->
      case content of
        (StatusCodeException _ body) ->
          case decode . LB.fromStrict $ body of
            Just er -> return $ Left er
            Nothing -> return . Left . err . B.unpack $ body
        ResponseTimeout -> return . Left . err $ "ResponseTimeout"
        other -> return . Left . err $ show other

    Left (InvalidUrlException _ _) ->
      return . Left . err $ "InvalidUrlException"
    Right r  -> return $ Right r

responseEither :: IO (Response a) -> IO (Either Err a)
responseEither req = do
  rsp <- tryResponse req
  case rsp of
    Left e  -> return $ Left e
    Right r -> return . Right $ r ^. responseBody

responseEither' :: IO (Response LB.ByteString) -> IO (Either Err ())
responseEither' req = do
  rsp <- tryResponse req
  case rsp of
    Left e  -> return $ Left e
    Right _ -> return $ Right ()

responseEitherJSON :: FromJSON a => IO (Response LB.ByteString) -> IO (Either Err a)
responseEitherJSON req = responseEither $ asJSON =<< req

responseJSON :: FromJSON a => IO (Response LB.ByteString) -> IO a
responseJSON = eitherToError . responseEitherJSON

responseOk :: FromJSON a => Text -> IO (Response LB.ByteString) -> IO (Either Err (Ok a))
responseOk okey req = do
  rsp <- responseEitherJSON req
  case rsp of
    Left e  -> return $ Left e
    Right r -> case toOk okey r of
                 Just v  -> return $ Right v
                 Nothing -> return . Left $ err "Invalid Result"

responseOk_ :: FromJSON a => Text -> IO (Response LB.ByteString) -> IO (Ok a)
responseOk_ okey req = eitherToError (responseOk okey req)

responseList :: FromJSON a => Text -> IO (Response LB.ByteString) -> IO (Either Err (List a))
responseList okey req = do
  rsp <- responseEitherJSON req
  case rsp of
    Left e  -> return $ Left e
    Right r -> case toList okey r of
                 Just v  -> return $ Right v
                 Nothing -> return . Left $ err "Invalid Result"

responseList_ :: FromJSON a => Text -> IO (Response LB.ByteString) -> IO (List a)
responseList_ okey req = eitherToError (responseList okey req)
