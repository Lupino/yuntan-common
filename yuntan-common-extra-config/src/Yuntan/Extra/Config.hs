{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}

module Yuntan.Extra.Config
  ( createConfigTable
  , initConfigState
  , setConfig_
  , getConfig_

  , setConfig
  , getConfig
  , getConfigJSON

  , setConfig'
  , getConfig'
  , getConfigJSON'
  , ConfigLru
  , fillValue
  , fillValue_
  ) where

import           Control.Concurrent.Async
import           Control.Concurrent.QSem
import qualified Control.Exception        (SomeException, bracket_, try)
import           Control.Monad            (void)
import           Data.Aeson               (FromJSON, Value, decodeStrict)
import           Data.ByteString          (ByteString)
import           Data.Hashable            (Hashable (..))
import           Data.Maybe               (listToMaybe)
import           Data.Pool                (withResource)
import           Data.String              (fromString)
import           Data.Typeable            (Typeable)
import           Database.MySQL.Simple    (Only (..), execute, execute_, query)
import           Haxl.Core                hiding (fetchReq)

import           Yuntan.Types.HasMySQL    (HasMySQL, MySQL, mysqlPool,
                                           tablePrefix)
import           Yuntan.Utils.JSON        (unionValue)
import           Yuntan.Utils.LruCache    (LruHandle, cached, remove)

createConfigTable :: MySQL ()
createConfigTable prefix conn = void $ execute_ conn sql
  where sql = fromString $ concat [ "CREATE TABLE IF NOT EXISTS `", prefix, "_config` ("
                                  , "  `key` varchar(128) NOT NULL,"
                                  , "  `value` TEXT DEFAULT NULL,"
                                  , "  PRIMARY KEY (`key`)"
                                  , ") ENGINE=InnoDB DEFAULT CHARSET=utf8"
                                  ]

setConfig_ :: String -> ByteString -> MySQL ()
setConfig_ key value prefix conn = void $ execute conn sql (key, value)
  where sql = fromString $ concat ["REPLACE INTO `", prefix, "_config` (`key`, `value`) VALUES (?, ?)"]

getConfig_ :: String -> MySQL (Maybe ByteString)
getConfig_ key prefix conn = fmap fromOnly . listToMaybe <$> query conn sql (Only key)
  where sql = fromString $ concat ["SELECT `value` FROM `", prefix, "_config` WHERE `key` = ?"]


-- Data source implementation.

data ConfigReq a where
  SetConfig :: String -> ByteString -> ConfigReq ()
  GetConfig :: String -> ConfigReq (Maybe ByteString)
  deriving (Typeable)

deriving instance Eq (ConfigReq a)
instance Hashable (ConfigReq a) where
  hashWithSalt s (SetConfig k v) = hashWithSalt s (1::Int, k, v)
  hashWithSalt s (GetConfig k)   = hashWithSalt s (2::Int, k)

deriving instance Show (ConfigReq a)
instance ShowP ConfigReq where showp = show

instance StateKey ConfigReq where
  data State ConfigReq = ConfigState { numThreads :: Int }

instance DataSourceName ConfigReq where
  dataSourceName _ = "ConfigDataSource"

instance HasMySQL u => DataSource u ConfigReq where
  fetch = doFetch

doFetch
  :: HasMySQL u
  => State ConfigReq
  -> Flags
  -> u
  -> PerformFetch ConfigReq

doFetch _state _flags _user = AsyncFetch $ \reqs inner -> do
  sem <- newQSem $ numThreads _state
  asyncs <- mapM (fetchAsync sem _user) reqs
  inner
  mapM_ wait asyncs

fetchAsync :: HasMySQL u => QSem -> u -> BlockedFetch ConfigReq -> IO (Async ())
fetchAsync sem env0 req = async $
  Control.Exception.bracket_ (waitQSem sem) (signalQSem sem) $ withResource pool $ fetchSync req prefix

  where pool   = mysqlPool env0
        prefix = tablePrefix env0

fetchSync :: BlockedFetch ConfigReq -> MySQL ()
fetchSync (BlockedFetch req rvar) prefix conn = do
  e <- Control.Exception.try $ fetchReq req prefix conn
  case e of
    Left ex -> putFailure rvar (ex :: Control.Exception.SomeException)
    Right a -> putSuccess rvar a

fetchReq :: ConfigReq a -> MySQL a
fetchReq (GetConfig k)   = getConfig_ k
fetchReq (SetConfig k v) = setConfig_ k v

initConfigState :: Int -> State ConfigReq
initConfigState = ConfigState

getConfig :: HasMySQL u => String -> GenHaxl u w (Maybe ByteString)
setConfig :: HasMySQL u => String -> ByteString -> GenHaxl u w ()
getConfig   = dataFetch . GetConfig
setConfig k = uncachedRequest . SetConfig k

getConfigJSON :: (HasMySQL u, FromJSON a) => String -> GenHaxl u w (Maybe a)
getConfigJSON k = maybe Nothing decodeStrict <$> getConfig k

type ConfigLru = Maybe (LruHandle String ByteString)

getConfig' :: HasMySQL u => (u -> ConfigLru) -> String -> GenHaxl u w (Maybe ByteString)
getConfig' lru k = cached lru k (getConfig k)

getConfigJSON' :: (HasMySQL u, FromJSON a) => (u -> ConfigLru) -> String -> GenHaxl u w (Maybe a)
getConfigJSON' lru k = maybe Nothing decodeStrict <$> cached lru k (getConfig k)

setConfig' :: HasMySQL u => (u -> ConfigLru) -> String -> ByteString -> GenHaxl u w ()
setConfig' lru k v = do
  remove lru k
  setConfig k v

fillValue
  :: HasMySQL u
  => (u -> ConfigLru)
  -> String
  -> (a -> Value)
  -> (Value -> a -> a)
  -> Maybe a
  -> GenHaxl u w (Maybe a)
fillValue _ _ _ _ Nothing    = return Nothing
fillValue lru k g f (Just v) = Just <$> fillValue_ lru k g f v

fillValue_
  :: HasMySQL u
  => (u -> ConfigLru)
  -> String
  -> (a -> Value)
  -> (Value -> a -> a)
  -> a
  -> GenHaxl u w a
fillValue_ lru k g f v = do
  ex <- getConfigJSON' lru k
  case ex of
    Nothing  -> return v
    Just ex' -> return $ f (unionValue (g v) ex') v

