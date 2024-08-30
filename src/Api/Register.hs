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
import Control.Monad.Reader (ask)
import Data.Aeson (FromJSON)
import Data.Password.Argon2
  ( hashPassword
  , mkPassword
  , unPasswordHash
  )
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Database.Schema 
  ( User (..)
  , UserId
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
import qualified Orville.PostgreSQL as O

type Register =
  "v1"
    :> "register"
    :> ReqBody '[JSON] RegisterRequest
    :> Post '[JSON] NoContent

data RegisterRequest = RegisterRequest
  { requestedUserId :: UserId
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

  pool <- ask
  userIdMatches <- liftIO $ O.runOrville pool $ do
    O.findEntity userTable requestedUserId
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
      liftIO $ O.runOrville pool $ do
        O.insertEntity userTable
          User
            { userId = requestedUserId
            , joined
            , password = hashedPassword
            }
      pure NoContent
