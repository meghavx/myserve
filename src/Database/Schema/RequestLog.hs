{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Database.Schema.RequestLog
  ( RequestLog (..)
  , requestLogTable
  , mkRequestLog
  ) where

import Data.Aeson (ToJSON)
import Data.ByteString.Char8 (ByteString, unpack)
import Data.Text (Text, pack)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import qualified Orville.PostgreSQL as O
import GHC.Generics (Generic)
import Network.Socket (SockAddr)
import Control.Monad.IO.Class (liftIO)
import Data.Time (UTCTime, getCurrentTime)
import Network.HTTP.Types (Method)

data RequestLog = RequestLog
  { logId :: UUID
  , method :: Maybe Text
  , path :: Text
  , clientAddr :: Text
  , callerUserId :: Maybe Text
  , loggedAt :: UTCTime
  }
  deriving (Show, Generic, ToJSON)

requestLogMarshaller :: O.SqlMarshaller RequestLog RequestLog
requestLogMarshaller = 
  RequestLog
    <$> O.marshallField logId logIdField
    <*> O.marshallField method methodField
    <*> O.marshallField path pathField
    <*> O.marshallField clientAddr clientAddrField
    <*> O.marshallField callerUserId callerUserIdField
    <*> O.marshallField loggedAt loggedAtField

logIdField :: O.FieldDefinition O.NotNull UUID
logIdField = 
  O.coerceField (O.uuidField "log_id")

methodField :: O.FieldDefinition O.Nullable (Maybe Text)
methodField = 
  O.nullableField (O.unboundedTextField "method")

pathField :: O.FieldDefinition O.NotNull Text
pathField = 
  O.unboundedTextField "path"

clientAddrField :: O.FieldDefinition O.NotNull Text
clientAddrField = 
  O.unboundedTextField "client_address"

callerUserIdField :: O.FieldDefinition O.Nullable (Maybe Text)
callerUserIdField = 
  O.nullableField (O.unboundedTextField "caller_user_id")

loggedAtField :: O.FieldDefinition O.NotNull UTCTime
loggedAtField = 
  O.utcTimestampField "logged_at"

requestLogTable :: O.TableDefinition (O.HasKey UUID) RequestLog RequestLog
requestLogTable = 
  O.mkTableDefinition 
    "request_log"
    (O.primaryKey logIdField)
    requestLogMarshaller

mkRequestLog 
  :: Method
  -> ByteString
  -> SockAddr
  -> Text
  -> IO RequestLog
mkRequestLog method' path' sockAddr userId = do
  logId <- nextRandom
  logTime <- liftIO getCurrentTime
  pure $
    RequestLog
      { logId
      , method = Just $ fromByteStringToText method'
      , path = fromByteStringToText path'
      , clientAddr = pack $ show sockAddr
      , callerUserId = Just userId
      , loggedAt = logTime
      }
    where fromByteStringToText = pack . unpack