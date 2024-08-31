{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Class (HasDb (..)) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ask)
import Orville.PostgreSQL 
  ( Orville
  , ConnectionPool
  , runOrville
  )

-- | A class that abstracts out database boilerplate
class HasDb m where
  -- | Run an Orville action
  runDb :: Orville a -> m a

instance (MonadReader ConnectionPool m, MonadIO m) => HasDb m where
  runDb query =
    ask >>= \pool -> liftIO $ runOrville pool query