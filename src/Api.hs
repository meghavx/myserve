{-# LANGUAGE TypeOperators #-}

module Api (Api, handlers) where

import Api.Register (Register, register)
import Api.Login (Login, login)
import Api.Greet (Greet, greet)
import Api.Logout (Logout, logout)
import Handler (MyServeHandler)
import Servant (ServerT, (:<|>) ((:<|>)))

type Api = 
  Register 
    :<|> Login 
    :<|> Greet 
    :<|> Logout

handlers :: ServerT Api MyServeHandler
handlers = 
  register 
    :<|> login 
    :<|> greet 
    :<|> logout
