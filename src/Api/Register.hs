{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Api.Register (Register, register) where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON)
import Data.Int (Int32)
import Data.Password.Argon2
  ( hashPassword
  , mkPassword
  , unPasswordHash
  )
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Database.Beam
  ( aggregate_
  , all_
  , as_
  , countAll_
  , filter_
  , insert
  , insertValues
  , runInsert
  , runSelectReturningOne
  , select
  , val_
  , (==.)
  )
import Database.Class (HasDb (runDb))
import Database.Schema (DevDb (users), UserT (..), devDb)
import GHC.Generics (Generic)
import Handler (MyServeHandler)
import Servant
  ( JSON
  , NoContent (NoContent)
  , Post
  , ReqBody
  , err412
  , err500
  , errBody
  , throwError
  , (:>)
  )

type Register =
  "v1"
    :> "register"
    :> ReqBody '[JSON] RegisterRequest
    :> Post '[JSON] NoContent

data RegisterRequest = RegisterRequest
  {requestedUserId :: Text, password :: Text}
  deriving (Generic, FromJSON)

register
  :: RegisterRequest -> MyServeHandler NoContent
register (RegisterRequest{..}) = do
  unless (T.length password > 8) $
    throwError $
      err412
        { errBody = "Password must be at least 8 digits in length."
        }
  userIdMatches <-
    runDb $
      runSelectReturningOne $
        select $
          aggregate_ (\_ -> as_ @Int32 countAll_) $
            filter_ (\user -> userId user ==. val_ requestedUserId) $
              all_ (users devDb)
  case userIdMatches of
    Nothing -> throwError err500
    Just 0 -> do
      hashedPassword <-
        fmap unPasswordHash <$> hashPassword $
          mkPassword password
      joined <- liftIO getCurrentTime
      runDb $
        runInsert $
          insert (users devDb) $
            insertValues
              [ User
                  { userId = requestedUserId
                  , joined
                  , password = hashedPassword
                  }
              ]
      pure NoContent
    Just _ ->
      throwError
        err412{errBody = "That user ID is already taken."}
