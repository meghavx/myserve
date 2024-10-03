{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Database.Schema.User (User (..), userTable) where

import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import qualified Orville.PostgreSQL as O

data User = User
  { userId :: Text
  , joined :: UTCTime
  , password :: Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)

userMarshaller :: O.SqlMarshaller User User
userMarshaller = 
  User
    <$> O.marshallField userId userIdField
    <*> O.marshallField joined joinedField
    <*> O.marshallField password passwordField

userIdField :: O.FieldDefinition O.NotNull Text
userIdField = 
  O.coerceField (O.unboundedTextField "user_id")

joinedField :: O.FieldDefinition O.NotNull UTCTime
joinedField = 
  O.utcTimestampField "joined"

passwordField :: O.FieldDefinition O.NotNull Text
passwordField = 
  O.unboundedTextField "password_argon2id"

userTable :: O.TableDefinition (O.HasKey Text) User User
userTable = 
  O.mkTableDefinition 
    "service_user"
    (O.primaryKey userIdField)
    userMarshaller