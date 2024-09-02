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
import Data.Password.Argon2
  ( hashPassword
  , mkPassword
  , unPasswordHash
  )
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Database.Class (HasDb (runDb))
import Database.Schema 
  ( User (..)
  , userTable
  )
import GHC.Generics (Generic)
import Handler (MyServeHandler)
import Servant
  ( JSON
  , NoContent (NoContent)
  , Post
  , ReqBody
  , err412
  , errBody
  , throwError
  , (:>)
  )
import Orville.PostgreSQL
  ( insertEntity
  , findEntity
  )

type Register =
  "v1"
    :> "register"
    :> ReqBody '[JSON] RegisterRequest
    :> Post '[JSON] NoContent

data RegisterRequest = RegisterRequest
  { requestedUserId :: Text
  , password :: Text 
  }
  deriving (Generic, FromJSON)

register
  :: RegisterRequest -> MyServeHandler NoContent
register (RegisterRequest{..}) = do
  unless (T.length password > 8) $
    throwError $
      err412
        { errBody = "Password must be at least 8 digits in length."
        }

  userIdMatches <- runDb $
    findEntity userTable requestedUserId
  case userIdMatches of
    Just _ -> do
      throwError $
        err412 
          { errBody = "That user ID is already taken." 
          }
    Nothing -> do
      joined <- liftIO getCurrentTime
      hashedPassword <-
        fmap unPasswordHash <$> hashPassword $
          mkPassword password
      runDb $
        insertEntity userTable
          User
            { userId = requestedUserId
            , joined
            , password = hashedPassword
            }
      pure NoContent
