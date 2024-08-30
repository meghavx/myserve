{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Handler (HandlerM, toServantHandler, MyServeHandler) where

import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader
  ( MonadReader
  , ReaderT
  , runReaderT
  )
import Orville.PostgreSQL.Raw.Connection (ConnectionPool)
import qualified Servant

-- | A monad for handlers to run in that has everything we need
type MyServeHandler = HandlerM (ConnectionPool)

-- | Servant.Handler with a dash of ReaderT
newtype HandlerM env a = HandlerM {runHandlerM :: ReaderT env Servant.Handler a}
  deriving newtype
    ( MonadIO
    , Functor
    , Applicative
    , Monad
    , MonadReader env
    , MonadError Servant.ServerError
    )

-- | Convert a HandlerM action to a Servant.Handler action
toServantHandler
  :: env -> HandlerM env a -> Servant.Handler a
toServantHandler env handler = runReaderT (runHandlerM handler) env
