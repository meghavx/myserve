{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api.Greet (Greet, greet) where

import Auth (PostAuth (KeepToken), UserId, WithTokenAuth)
import Data.Aeson (ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Handler (MyServeHandler)
import LogRequest (LogMode (..), LogRequest)
import Servant (Get, JSON, (:>))

type Greet =
  "v1"
    :> "greet"
    :> LogRequest '[StdoutLog, DbLog]
    :> WithTokenAuth KeepToken
    :> Get '[JSON] GreetResponse

data GreetResponse = GreetResponse 
  { welcomeMessage :: Text
  }
  deriving (Generic, ToJSON)

-- | Greet a user
greet :: UserId -> MyServeHandler GreetResponse
greet userId = pure $ GreetResponse $ "Hello, " <> userId <> "!"