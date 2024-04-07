{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Database.Schema.AuthToken (AuthTokenT (..), AuthToken) where

import Data.Functor.Identity (Identity)
import Data.Text (Text)
import Data.Time (LocalTime)
import Database.Beam
  ( Beamable
  , Columnar
  , PrimaryKey
  , Table (primaryKey)
  )
import GHC.Generics

data AuthTokenT f = AuthToken
  { authToken :: Columnar f Text
  , created :: Columnar f LocalTime
  }
  deriving (Generic)

type AuthToken = AuthTokenT Identity

deriving instance Show AuthToken

instance Beamable AuthTokenT

instance Table AuthTokenT where
  data PrimaryKey AuthTokenT f = AuthTokenId (Columnar f Text)
    deriving (Generic, Beamable)
  primaryKey = AuthTokenId . authToken
