{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Database.Schema.Member (MemberT (..), Member) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Functor.Identity (Identity)
import Data.Text (Text)
import Data.Time (Day)
import Database.Beam
  ( Beamable
  , Columnar
  , PrimaryKey
  , Table (primaryKey)
  )
import GHC.Generics

data MemberT f = Member
  { email :: Columnar f Text
  , firstName :: Columnar f Text
  , lastName :: Columnar f Text
  , dateOfBirth :: Columnar f Day
  , isPermanentMember :: Columnar f Bool
  , correspondenceAddress :: Columnar f (Maybe Text)
  }
  deriving (Generic)

type Member = MemberT Identity

instance Beamable MemberT

deriving instance Show Member

deriving instance ToJSON Member

deriving instance FromJSON Member

instance Table MemberT where
  data PrimaryKey MemberT f = MemberEmail (Columnar f Text)
    deriving (Generic, Beamable)
  primaryKey = MemberEmail . email
