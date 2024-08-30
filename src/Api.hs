module Api (Api, handlers) where

import Api.Register (Register, register)
import Handler (MyServeHandler)
import Servant (ServerT)

type Api = Register

handlers :: ServerT Api MyServeHandler
handlers = register
