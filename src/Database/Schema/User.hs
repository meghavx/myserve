{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Database.Schema.User (UserT (..), User, UserId) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Functor.Identity (Identity)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Beam
  ( Beamable
  , Columnar
  , Table (PrimaryKey, primaryKey)
  )
import GHC.Generics (Generic)

data UserT f = User
  { userId :: Columnar f Text
  , joined :: Columnar f UTCTime
  , password :: Columnar f Text
  }
  deriving (Generic)

type User = UserT Identity

instance Beamable UserT

deriving instance Show User

deriving instance ToJSON User

deriving instance FromJSON User

instance Table UserT where
  data PrimaryKey UserT f = UserId (Columnar f Text)
    deriving (Generic, Beamable)
  primaryKey = UserId . userId

type UserId = PrimaryKey UserT Identity

deriving instance Show UserId
