{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Data.ByteString (ByteString)
import Data.Pool (createPool)
import Data.Proxy (Proxy (Proxy))
import Database.PostgreSQL.Simple (close, connectPostgreSQL)
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (run)
import Servant (Context (..), serveWithContext)
import Server (Api, server)
import System.Envy (FromEnv (..), decodeEnv)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

data Env = Env
  { serverPort :: Int
  , pgConnectString :: ByteString
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
    createPool
      (connectPostgreSQL pgConnectString)
      close
      1 -- stripes
      60 -- keep alive
      10 -- connections per stripe
  run serverPort $
    Servant.serveWithContext
      (Proxy @Api)
      (pool :. authTokenTimeoutSeconds :. EmptyContext)
      (server pool)
