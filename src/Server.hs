{-# LANGUAGE DataKinds #-}

module Server
  ( Api
  , server
  ) where

import Api (Api, handlers)
import Data.Proxy (Proxy (Proxy))

import Orville.PostgreSQL.Raw.Connection (ConnectionPool)
import Handler (toServantHandler)
import Servant (Server)
import Servant.Server.Internal
  ( HasServer (hoistServerWithContext)
  )

server :: ConnectionPool -> Server Api
server pool =
  hoistServerWithContext
    (Proxy :: Proxy Api)
    (Proxy :: Proxy '[ConnectionPool, Integer])
    (toServantHandler pool)
    handlers
