{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Data.Proxy (Proxy (Proxy))
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (run)
import Servant (Context (..), serveWithContext)
import Server (Api, server)
import System.Envy (FromEnv (..), decodeEnv)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import qualified Orville.PostgreSQL as O
import Database.Schema (migrateSchema)

data Env = Env
  { serverPort :: Int
  , pgConnectString :: String
  , authTokenTimeoutSeconds :: Integer
  }
  deriving (Generic, FromEnv)

main :: IO ()
main = do
  Env{..} <-
    decodeEnv
      >>= flip
        either
        pure
        ( \envError -> do
            hPutStrLn stderr $
              "Failed to parse environment. " <> envError
            exitFailure
        )
  pool <-
    O.createConnectionPool
      O.ConnectionOptions
        { O.connectionString = pgConnectString
        , O.connectionNoticeReporting = O.DisableNoticeReporting
        , O.connectionPoolStripes = O.OneStripePerCapability
        , O.connectionPoolMaxConnections = O.MaxConnectionsPerStripe 10
        , O.connectionPoolLingerTime = 60
        }
   -- Run database migration
  O.runOrville pool migrateSchema

  run serverPort $
    Servant.serveWithContext
      (Proxy @Api)
      (pool :. authTokenTimeoutSeconds :. EmptyContext)
      (server pool)
