{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.Logout (Logout, logout) where

import Auth (PostAuth (DiscardToken), UserId, WithTokenAuth)
import Handler (MyServeHandler)
import Servant
  ( Get
  , JSON
  , NoContent (NoContent)
  , (:>)
  )

type Logout =
  "v1"
    :> "logout"
    :> WithTokenAuth DiscardToken
    :> Get '[JSON] NoContent

logout :: UserId -> MyServeHandler NoContent
logout _ = pure NoContent