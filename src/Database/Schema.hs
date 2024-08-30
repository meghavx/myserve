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
import Database.Schema.User as X 
  ( User
  , UserId
  , UserT (..)
  )
import GHC.Generics (Generic)

data DevDb f = DevDb
  { 
    users :: f (TableEntity UserT)
  }
  deriving (Generic, Database be)

devDb :: DatabaseSettings be DevDb
devDb =
  defaultDbSettings
    `withDbModification` dbModification
      { users =
          setEntityName "service_user"
            <> modifyTableFields
              tableModification
                { userId = "user_id"
                , password = "password_argon2id"
                }
      }
