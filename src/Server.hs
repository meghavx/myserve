{-# LANGUAGE DataKinds #-}

module Server
  ( Api
  , server
  ) where

import Api (Api, handlers)
import Data.Pool (Pool)
import Data.Proxy (Proxy (Proxy))
import Database.PostgreSQL.Simple (Connection)
import Handler (toServantHandler)
import Servant (Server)
import Servant.Server.Internal
  ( HasServer (hoistServerWithContext)
  )

server :: Pool Connection -> Server Api
server pool =
  hoistServerWithContext
    (Proxy :: Proxy Api)
    (Proxy :: Proxy '[Pool Connection, Integer])
    (toServantHandler pool)
    handlers
