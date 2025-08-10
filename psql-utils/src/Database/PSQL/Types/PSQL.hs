module Database.PSQL.Types.PSQL
  ( createPSQLPool
  , PSQL
  , runPSQL
  , runPSQLPool
  , runPSQLEnv

  , getTablePrefix

  , withTransaction
  , execute
  , execute_
  , query
  , query_
  ) where


import           Control.Monad.IO.Class             (MonadIO (..))
import           Data.ByteString                    (ByteString)
import           Data.Int                           (Int64)
import           Data.Pool                          (defaultPoolConfig, newPool,
                                                     setNumStripes,
                                                     withResource)
import           Database.PostgreSQL.Simple         (Connection, Query, close,
                                                     connectPostgreSQL)
import qualified Database.PostgreSQL.Simple         as L (execute, execute_,
                                                          query, query_,
                                                          withTransaction)
import           Database.PostgreSQL.Simple.FromRow (FromRow (..))
import           Database.PostgreSQL.Simple.ToRow   (ToRow (..))
import           Database.PSQL.Class.HasPSQL        (HasPSQL (..), PSQLPool)
import           Database.PSQL.Types.TablePrefix    (TablePrefix)

createPSQLPool :: ByteString -> Maybe Int -> Double -> Int -> IO PSQLPool
createPSQLPool path numStripes idleTime =
  newPool . setNumStripes numStripes . defaultPoolConfig (connectPostgreSQL path) close idleTime

newtype PSQL a = PSQL {unPSQL :: TablePrefix -> Connection -> IO a}

instance Functor PSQL where
  fmap f a = PSQL $ \p c -> f <$> unPSQL a p c
  {-# INLINE fmap #-}

instance Applicative PSQL where
  pure a = PSQL $ \_ _ -> pure a
  {-# INLINE pure #-}
  f <*> v = PSQL $ \p c -> unPSQL f p c <*> unPSQL v p c
  {-# INLINE (<*>) #-}


instance Monad PSQL where
  m >>= k = PSQL $ \p c -> do
    a <- unPSQL m p c
    unPSQL (k a) p c
  {-# INLINE (>>=) #-}

instance MonadIO PSQL where
  liftIO m = PSQL $ \_ _ -> m
  {-# INLINE liftIO #-}

runPSQL :: TablePrefix -> Connection -> PSQL a -> IO a
runPSQL prefix conn m = unPSQL m prefix conn

runPSQLPool :: TablePrefix -> PSQLPool -> PSQL a -> IO a
runPSQLPool prefix pool m = withResource pool $ unPSQL m prefix

getTablePrefix :: PSQL TablePrefix
getTablePrefix = PSQL $ \p _ -> return p

runPSQLEnv :: (HasPSQL env) => env -> PSQL a -> IO a
runPSQLEnv env = runPSQLPool (tablePrefix env) (psqlPool env)

withTransaction :: PSQL a -> PSQL a
withTransaction m = PSQL $ \prefix conn ->
  L.withTransaction conn $ runPSQL prefix conn m

execute_ :: (TablePrefix -> Query) -> PSQL Int64
execute_ sql = PSQL $ \prefix conn -> L.execute_ conn (sql prefix)

execute :: ToRow a => (TablePrefix -> Query) -> a -> PSQL Int64
execute sql a = PSQL $ \prefix conn -> L.execute conn (sql prefix) a


query_ :: FromRow b => (TablePrefix -> Query) -> PSQL [b]
query_ sql = PSQL $ \prefix conn -> L.query_ conn (sql prefix)

query :: (ToRow a, FromRow b) => (TablePrefix -> Query) -> a -> PSQL [b]
query sql a = PSQL $ \prefix conn -> L.query conn (sql prefix) a
