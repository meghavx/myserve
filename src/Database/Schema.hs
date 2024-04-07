{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Schema (MemberT (..), Member, AuthTokenT (..), AuthToken, DevDb (..), devDb) where

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
import Database.Schema.AuthToken (AuthToken, AuthTokenT (..))
import Database.Schema.Member (Member, MemberT (..))
import GHC.Generics

data DevDb f = DevDb
  { members :: f (TableEntity MemberT)
  , authTokens :: f (TableEntity AuthTokenT)
  }
  deriving (Generic, Database be)

devDb :: DatabaseSettings be DevDb
devDb =
  defaultDbSettings
    `withDbModification` dbModification
      { members =
          setEntityName "member"
            <> modifyTableFields
              tableModification
                { email = "email"
                , firstName = "first_name"
                , lastName = "last_name"
                , dateOfBirth = "date_of_birth"
                , isPermanentMember = "permanent"
                , correspondenceAddress = "correspondence_address"
                }
      , authTokens =
          setEntityName "auth_token"
            <> modifyTableFields
              tableModification{authToken = "auth_token", created = "created"}
      }
