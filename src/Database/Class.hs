{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Class (HasDb (..)) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ask)
import Data.Pool
import Database.Beam.Postgres (Pg, runBeamPostgres)
import Database.PostgreSQL.Simple (Connection)

-- | A class that abstracts out database boilerplate
class HasDb m where
  -- | Run a Beam action with the Postgres backend
  runDb :: Pg a -> m a

instance (MonadReader (Pool Connection) m, MonadIO m) => HasDb m where
  runDb pg =
    ask >>= \pool -> liftIO . withResource pool $ \conn -> runBeamPostgres conn pg
