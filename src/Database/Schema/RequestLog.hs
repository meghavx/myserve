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

module Database.Schema.RequestLog (RequestLog (..), requestLogTable, mkRequestLog) where

import Data.Aeson (FromJSON, ToJSON, encode, decode)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict, fromStrict)
import qualified Data.ByteString.Char8 as BS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import qualified Orville.PostgreSQL as O
import qualified Orville.PostgreSQL.Raw.SqlValue as SqlValue
import GHC.Generics (Generic)
import Network.Socket (SockAddr)
import Control.Monad.IO.Class (liftIO)
import Data.Time (UTCTime, getCurrentTime)
import Network.HTTP.Types (Method)
import Network.HTTP.Types.Header (RequestHeaders)
import Data.CaseInsensitive (original)
import Data.Maybe (fromMaybe)

data RequestHeader = RequestHeader 
  { name :: String
  , value :: String
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data RequestLog = RequestLog
  { logId :: UUID
  , method :: Maybe Text
  , path :: Text
  , clientAddr :: Text
  , callerUserId :: Maybe Text
  , headers :: Maybe [RequestHeader]
  , loggedAt :: UTCTime
  }
  deriving (Show, Generic, ToJSON, FromJSON)

requestLogMarshaller :: O.SqlMarshaller RequestLog RequestLog
requestLogMarshaller = 
  RequestLog
    <$> O.marshallField logId logIdField
    <*> O.marshallField method methodField
    <*> O.marshallField path pathField
    <*> O.marshallField clientAddr clientAddrField
    <*> O.marshallField callerUserId callerUserIdField
    <*> O.marshallField headers headersField
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

headersField :: O.FieldDefinition O.Nullable (Maybe [RequestHeader])
headersField = 
  O.nullableField (myJsonbField toSql fromSql "headers")
    where
      toSql = SqlValue.fromText . decodeUtf8 . toStrict . encode
      fromSql = fmap (fromMaybe [] . decode . fromStrict . encodeUtf8) . SqlValue.toText

myJsonbField 
  :: (a -> SqlValue.SqlValue) 
  -> (SqlValue.SqlValue -> Either String a) 
  -> String 
  -> O.FieldDefinition O.NotNull a
myJsonbField toSql fromSql fieldName =
  O.fieldOfType
    (O.jsonb
      { O.sqlTypeToSql = toSql
      , O.sqlTypeFromSql = fromSql
      }
    )
    fieldName

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
  -> RequestHeaders
  -> IO RequestLog
mkRequestLog method' path' sockAddr' userId' headers' = do
  logId <- nextRandom
  logTime <- liftIO getCurrentTime
  pure $
    RequestLog
      { logId
      , method = Just $ decodeUtf8 method'
      , path = decodeUtf8 path'
      , clientAddr = T.pack $ show sockAddr'
      , callerUserId = Just userId'
      , headers = Just $ toRequestHeaders headers'
      , loggedAt = logTime
      }
    where 
      toRequestHeaders = fmap $
        \(k, v) -> 
          RequestHeader 
            (BS.unpack $ original k)
            (BS.unpack v)

