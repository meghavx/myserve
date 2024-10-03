{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Database.Schema.AuthToken
  ( AuthToken (..)
  , authTokenTable
  , createdByField
  ) where

import Data.Aeson (ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import qualified Orville.PostgreSQL as O

data AuthToken = AuthToken
  { token :: UUID
  , createdAt :: UTCTime
  , createdBy :: Text
  }
  deriving (Show, Generic, ToJSON)

authTokenMarshaller :: O.SqlMarshaller AuthToken AuthToken
authTokenMarshaller = 
  AuthToken
    <$> O.marshallField token tokenField
    <*> O.marshallField createdAt createdAtField
    <*> O.marshallField createdBy createdByField

tokenField :: O.FieldDefinition O.NotNull UUID
tokenField = 
  O.coerceField (O.uuidField "auth_token")

createdAtField :: O.FieldDefinition O.NotNull UTCTime
createdAtField = 
  O.utcTimestampField "created_at"

createdByField :: O.FieldDefinition O.NotNull Text
createdByField = 
  O.unboundedTextField "created_by"

authTokenTable :: O.TableDefinition (O.HasKey UUID) AuthToken AuthToken
authTokenTable = 
  O.mkTableDefinition 
    "auth_token"
    (O.primaryKey tokenField)
    authTokenMarshaller