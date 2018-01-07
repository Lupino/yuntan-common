{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Yuntan.Utils.Wreq
  (
    responseValue
  , responseMaybe
  , responseEither
  , responseEither'
  , responseEitherJSON
  , responseJSON
  , responseOkResult
  , responseOkResult_
  , responseListResult
  , responseListResult_
  , tryResponse
  , Error
  , throwError
  , errorMsg
  , eitherToError
  ) where

import           Control.Exception       (Exception, throwIO, try)
import           Control.Lens            ((^.), (^?))
import           Data.Aeson              (FromJSON (..), decode)
import qualified Data.ByteString.Char8   as B (unpack)
import qualified Data.ByteString.Lazy    as LB (ByteString, fromStrict)
import           Data.Text               (Text)
import           Network.HTTP.Client     (HttpException (..),
                                          HttpExceptionContent (..))
import           Network.Wreq            (Response, asJSON, responseBody)
import           Yuntan.Types.ListResult (ListResult, emptyListResult,
                                          toListResult)
import           Yuntan.Types.Result     (ErrResult (..), OkResult, err,
                                          toOkResult)

newtype Error = Error String
  deriving (Show, Eq, Ord)

instance Exception Error

throwError :: String -> IO a
throwError e = throwIO $ Error e

eitherToError :: IO (Either ErrResult a) -> IO a
eitherToError io  = do
  r <- io
  case r of
    Left e  -> throwError $ errMsg e
    Right v -> pure v

errorMsg :: Error -> String
errorMsg (Error e) = e

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

tryResponse :: IO (Response a) -> IO (Either ErrResult (Response a))
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

responseEither :: IO (Response a) -> IO (Either ErrResult a)
responseEither req = do
  rsp <- tryResponse req
  case rsp of
    Left e  -> return $ Left e
    Right r -> return . Right $ r ^. responseBody

responseEither' :: IO (Response LB.ByteString) -> IO (Either ErrResult ())
responseEither' req = do
  rsp <- tryResponse req
  case rsp of
    Left e  -> return $ Left e
    Right _ -> return $ Right ()

responseEitherJSON :: FromJSON a => IO (Response LB.ByteString) -> IO (Either ErrResult a)
responseEitherJSON req = responseEither $ asJSON =<< req

responseJSON :: FromJSON a => IO (Response LB.ByteString) -> IO a
responseJSON = eitherToError . responseEitherJSON

responseOkResult :: FromJSON a => Text -> IO (Response LB.ByteString) -> IO (Either ErrResult (OkResult a))
responseOkResult okey req = do
  rsp <- responseEitherJSON req
  case rsp of
    Left e  -> return $ Left e
    Right r -> case toOkResult okey r of
                 Just v  -> return $ Right v
                 Nothing -> return . Left $ err "Invalid Result"

responseOkResult_ :: FromJSON a => Text -> IO (Response LB.ByteString) -> IO (OkResult a)
responseOkResult_ okey req = eitherToError (responseOkResult okey req)

responseListResult :: FromJSON a => Text -> IO (Response LB.ByteString) -> IO (Either ErrResult (ListResult a))
responseListResult okey req = do
  rsp <- responseEitherJSON req
  case rsp of
    Left e  -> return $ Left e
    Right r -> case toListResult okey r of
                 Just v  -> return $ Right v
                 Nothing -> return . Left $ err "Invalid Result"

responseListResult_ :: FromJSON a => Text -> IO (Response LB.ByteString) -> IO (ListResult a)
responseListResult_ okey req = eitherToError (responseListResult okey req)
