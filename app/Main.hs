module Main (main) where

import Data.Pool
import Data.Proxy
import Database.PostgreSQL.Simple (ConnectInfo (..), close, connect)
import Network.Wai.Handler.Warp (run)
import Servant (Context (..), serveWithContext)
import Server (Api, server)

connectionSettings :: ConnectInfo
connectionSettings =
  ConnectInfo
    { connectHost = "localhost"
    , connectPort = 54321
    , connectUser = "postgres"
    , connectPassword = "password"
    , connectDatabase = "devdb"
    }

main :: IO ()
main = do
  pool <- createPool (connect connectionSettings) close 2 60 10
  run 8080 (app pool)
 where
  app ctx =
    Servant.serveWithContext (Proxy :: Proxy Api) (ctx :. EmptyContext) (server ctx)
