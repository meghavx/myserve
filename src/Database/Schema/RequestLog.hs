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

data RequestLog = RequestLog
  { logId :: UUID
  , clientAddr :: Text
  , path :: Text
  , loggedAt :: UTCTime
  }
  deriving (Show, Generic, ToJSON)

requestLogMarshaller :: O.SqlMarshaller RequestLog RequestLog
requestLogMarshaller = 
  RequestLog
    <$> O.marshallField logId logIdField
    <*> O.marshallField clientAddr clientAddrField
    <*> O.marshallField path pathField
    <*> O.marshallField loggedAt loggedAtField

logIdField :: O.FieldDefinition O.NotNull UUID
logIdField = 
  O.coerceField (O.uuidField "log_id")

clientAddrField :: O.FieldDefinition O.NotNull Text
clientAddrField = 
  O.unboundedTextField "client_address"

pathField :: O.FieldDefinition O.NotNull Text
pathField = 
  O.unboundedTextField "path"

loggedAtField :: O.FieldDefinition O.NotNull UTCTime
loggedAtField = 
  O.utcTimestampField "logged_at"

requestLogTable :: O.TableDefinition (O.HasKey UUID) RequestLog RequestLog
requestLogTable = 
  O.mkTableDefinition 
    "request_log"
    (O.primaryKey logIdField)
    requestLogMarshaller

mkRequestLog :: SockAddr -> ByteString -> IO RequestLog
mkRequestLog sockAddr path' = do
  logId <- nextRandom
  logTime <- liftIO getCurrentTime
  pure $
    RequestLog
      { logId
      , clientAddr = pack $ show sockAddr
      , path = pack $ unpack path'
      , loggedAt = logTime
      }