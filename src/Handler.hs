{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Handler (Handler, toServantHandler) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Data.Pool
import Database.PostgreSQL.Simple (Connection)
import qualified Servant

-- | The type that all our handlers will run in.
newtype Handler a = Handler {runHandler :: ReaderT (Pool Connection) Servant.Handler a}
  deriving newtype
    (MonadIO, Functor, Applicative, Monad, MonadReader (Pool Connection))

-- | Convert an action in our Handler type to one in Servant's Handler type.
toServantHandler :: Pool Connection -> Handler a -> Servant.Handler a
toServantHandler pool handler = runReaderT (runHandler handler) pool
