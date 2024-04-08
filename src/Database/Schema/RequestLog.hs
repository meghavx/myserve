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

module Database.Schema.RequestLog (RequestLogT (..), RequestLog, mkRequestLog) where

import Data.Aeson (FromJSON, ToJSON (toJSON), object, (.=))
import Data.ByteString.Char8 (ByteString, unpack)
import Data.CaseInsensitive (original)
import Data.Functor.Identity (Identity)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Database.Beam (Beamable, Columnar, Table (PrimaryKey, primaryKey))
import Database.Beam.Postgres (PgJSON (..))
import GHC.Generics (Generic)
import qualified Network.HTTP.Types.Header as Network
import Network.Socket (SockAddr)

data RequestHeader = RequestHeader {name :: String, value :: String}
  deriving (Generic, Show, ToJSON, FromJSON)

data RequestLogT f = RequestLog
  { logId :: Columnar f UUID
  , clientAddr :: Columnar f String
  , headers :: Columnar f (PgJSON [RequestHeader])
  , path :: Columnar f String
  }
  deriving (Generic)

instance ToJSON RequestLog where
  toJSON (RequestLog logId' clientAddr' (PgJSON headers') path') =
    object
      [ "logId" .= logId'
      , "clientAddr" .= clientAddr'
      , "headers" .= headers'
      , "path" .= path'
      ]

mkRequestLog
  :: SockAddr -> Network.RequestHeaders -> ByteString -> IO RequestLog
mkRequestLog sockAddr headers' path' = do
  logId <- nextRandom
  pure $
    RequestLog
      { logId
      , clientAddr = show sockAddr
      , headers = PgJSON $ toRequestHeaders headers'
      , path = unpack path'
      }
 where
  toRequestHeaders =
    fmap
      (\(k, v) -> RequestHeader (unpack $ original k) (unpack v))

type RequestLog = RequestLogT Identity

deriving instance Show RequestLog

instance Beamable RequestLogT

instance Table RequestLogT where
  data PrimaryKey RequestLogT f = RequestLogId (Columnar f UUID)
    deriving (Generic, Beamable)
  primaryKey = RequestLogId . logId
