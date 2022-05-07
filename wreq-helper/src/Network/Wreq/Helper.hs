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


import           Control.Exception     (Exception, throwIO, try)
import           Control.Monad         (unless)
import           Data.Aeson            (FromJSON (..), decode, eitherDecode')
import           Data.Aeson.Key        (Key)
import           Data.Aeson.Result     (Err, List, Ok, err, throwError, toList,
                                        toOk)
import qualified Data.ByteString       as B (break, isPrefixOf, isSuffixOf)
import qualified Data.ByteString.Char8 as B (unpack)
import qualified Data.ByteString.Lazy  as LB (ByteString, fromStrict)
import           Data.Maybe            (fromMaybe)
import           Network.HTTP.Client   (HttpException (..),
                                        HttpExceptionContent (..), Response,
                                        responseBody, responseHeaders)

data JSONError = JSONError String
  deriving (Show)

instance Exception JSONError

asJSON :: FromJSON a => Response LB.ByteString -> IO (Response a)
asJSON resp = do
  let contentType = fst . B.break (==59) . fromMaybe "unknown" .
                    lookup "Content-Type" . responseHeaders $ resp
  unless ("application/json" `B.isPrefixOf` contentType
        || ("application/" `B.isPrefixOf` contentType && "+json" `B.isSuffixOf` contentType)) $
    throwIO . JSONError $ "content type of response is " ++ show contentType
  case eitherDecode' (responseBody resp) of
    Left e    -> throwIO (JSONError e)
    Right val -> return (fmap (const val) resp)

eitherToError :: IO (Either Err a) -> IO a
eitherToError io  = do
  r <- io
  case r of
    Left e  -> throwError e
    Right v -> pure v

responseValue :: IO (Response a) -> IO a
responseValue req = responseBody <$> req

responseMaybe :: IO (Response a) -> IO (Maybe a)
responseMaybe req = do
  e <- try req
  case e of
    Left (_ :: HttpException) -> return Nothing
    Right r                   -> return . Just $ responseBody r

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
    Right r -> return . Right $ responseBody r

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

responseOk
  :: FromJSON a
  => Key -> IO (Response LB.ByteString) -> IO (Either Err (Ok a))
responseOk okey req = do
  rsp <- responseEitherJSON req
  case rsp of
    Left e  -> return $ Left e
    Right r -> case toOk okey r of
                 Just v  -> return $ Right v
                 Nothing -> return . Left $ err "Invalid Result"

responseOk_
  :: FromJSON a
  => Key -> IO (Response LB.ByteString) -> IO (Ok a)
responseOk_ okey req = eitherToError (responseOk okey req)

responseList
  :: FromJSON a
  => Key -> IO (Response LB.ByteString) -> IO (Either Err (List a))
responseList okey req = do
  rsp <- responseEitherJSON req
  case rsp of
    Left e  -> return $ Left e
    Right r -> case toList okey r of
                 Just v  -> return $ Right v
                 Nothing -> return . Left $ err "Invalid Result"

responseList_
  :: FromJSON a
  => Key -> IO (Response LB.ByteString) -> IO (List a)
responseList_ okey req = eitherToError (responseList okey req)
