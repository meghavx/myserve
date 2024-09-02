{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Api.Login (Login, login) where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson 
  ( FromJSON (..)
  , ToJSON
  , withObject
  , (.:)
  )
import Data.Maybe (isJust)
import Data.Password.Argon2
  ( PasswordCheck (..)
  , PasswordHash (PasswordHash)
  , checkPassword
  , mkPassword
  )
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Data.UUID (UUID)
import qualified Data.UUID.V4 as UuidV4
import Orville.PostgreSQL 
  ( insertEntity
  , findEntity
  , findFirstEntityBy
  , where_
  , (.==)
  )
import Database.Class (HasDb (runDb))
import Database.Schema
  ( User (..)
  , userTable
  , AuthToken (..)
  , authTokenTable
  , createdByField
  )
import GHC.Generics (Generic)
import Handler (MyServeHandler)
import Servant
  ( JSON
  , Post
  , ReqBody
  , err400
  , err403
  , throwError
  , (:>)
  )

type Login =
  "v1"
    :> "login"
    :> ReqBody '[JSON] LoginRequest
    :> Post '[JSON] LoginResponse

data LoginRequest = LoginRequest 
  { userId' :: Text
  , password' :: Text
  }

instance FromJSON LoginRequest where
  parseJSON = withObject "LoginRequest" $ 
    \o ->
      pure LoginRequest 
        <*> (o .: "userId") 
        <*> (o .: "password")

data LoginResponse = LoginResponse 
  { authToken :: UUID
  }
  deriving (Generic, ToJSON)

login :: LoginRequest -> MyServeHandler LoginResponse
login LoginRequest {..} = do
  userLoggedIn <-
    fmap hasActiveToken $
      runDb $
        findFirstEntityBy authTokenTable $
          where_ $ createdByField .== userId'
  when userLoggedIn $ throwError err400
  userMay <-
    runDb $
      findEntity userTable userId'
  case userMay of
    Nothing -> throwError err400
    Just (PasswordHash . password -> hashedPassword) -> do
      let checkResult = checkPassword (mkPassword password') hashedPassword
      case checkResult of
        PasswordCheckFail -> throwError err403
        PasswordCheckSuccess -> do
          token <- liftIO UuidV4.nextRandom
          createdAt <- liftIO getCurrentTime
          runDb $
            insertEntity authTokenTable
              AuthToken
                { token = token
                , createdAt
                , createdBy = userId'
                }
          pure $ LoginResponse token
  where
    hasActiveToken = isJust       