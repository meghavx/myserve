{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Schema
  ( module X
  , DevDb (..)
  , devDb
  ) where

import Database.Beam
  ( Database
  , DatabaseSettings
  , TableEntity
  , dbModification
  , defaultDbSettings
  , modifyTableFields
  , setEntityName
  , tableModification
  , withDbModification
  )
import Database.Schema.AuthToken as X
  ( AuthToken
  , AuthTokenT (..)
  , Token
  )
import Database.Schema.RequestLog as X
  ( RequestLog
  , RequestLogT (..)
  )
import Database.Schema.User as X 
  ( User
  , UserId
  , UserT (..)
  )
import GHC.Generics (Generic)

data DevDb f = DevDb
  { authTokens :: f (TableEntity AuthTokenT)
  , requestLogs :: f (TableEntity RequestLogT)
  , users :: f (TableEntity UserT)
  }
  deriving (Generic, Database be)

devDb :: DatabaseSettings be DevDb
devDb =
  defaultDbSettings
    `withDbModification` dbModification
      { authTokens =
          setEntityName "auth_token"
            <> modifyTableFields
              tableModification
                { token = "auth_token"
                , createdAt = "created_at"
                , createdBy = "created_by"
                }
      , requestLogs =
          setEntityName "request_log"
            <> modifyTableFields
              tableModification
                { logId = "log_id"
                , clientAddr = "client_address"
                , loggedAt = "logged_at"
                }
      , users =
          setEntityName "service_user"
            <> modifyTableFields
              tableModification
                { userId = "user_id"
                , password = "password_argon2id"
                }
      }
