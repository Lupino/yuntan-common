{-# LANGUAGE MultiParamTypeClasses #-}

module Database.PSQL.Class.HasOtherEnv
  ( HasOtherEnv (..)
  ) where


class HasOtherEnv u a where
  otherEnv :: a -> u
