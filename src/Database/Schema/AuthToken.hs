{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Database.Schema.AuthToken (AuthTokenT (..), AuthToken, Token) where

import Data.Aeson (ToJSON)
import Data.Functor.Identity (Identity)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Database.Beam
  ( Beamable
  , Columnar
  , PrimaryKey
  , Table (primaryKey)
  )
import GHC.Generics (Generic)

data AuthTokenT f = AuthToken
  { token :: Columnar f UUID
  , createdAt :: Columnar f UTCTime
  , createdBy :: Columnar f Text
  }
  deriving (Generic)

type AuthToken = AuthTokenT Identity

deriving instance Show AuthToken

instance Beamable AuthTokenT

instance Table AuthTokenT where
  data PrimaryKey AuthTokenT f = Token (Columnar f UUID)
    deriving (Generic, Beamable)
  primaryKey = Token . token

type Token = PrimaryKey AuthTokenT Identity

deriving instance Show Token

deriving instance ToJSON Token
