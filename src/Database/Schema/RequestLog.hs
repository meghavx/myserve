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

import Data.Aeson (ToJSON (toJSON), object, (.=))
import Data.ByteString.Char8 (ByteString, unpack)
import Data.Functor.Identity (Identity)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Database.Beam (Beamable, Columnar, Table (PrimaryKey, primaryKey))
import GHC.Generics (Generic)
import Network.Socket (SockAddr)
import Control.Monad.IO.Class (liftIO)
import Data.Time (getCurrentTime, UTCTime)

data RequestLogT f = RequestLog
  { logId :: Columnar f UUID
  , clientAddr :: Columnar f String
  , path :: Columnar f String
  , loggedAt :: Columnar f UTCTime
  }
  deriving (Generic)

instance ToJSON RequestLog where
  toJSON (RequestLog logId' clientAddr' path' loggedAt') =
    object
      [ "logId" .= logId'
      , "clientAddr" .= clientAddr'
      , "path" .= path'
      , "loggedAt" .= loggedAt'
      ]

mkRequestLog
  :: SockAddr -> ByteString -> IO RequestLog
mkRequestLog sockAddr path' = do
  logId <- nextRandom
  logTime <- liftIO getCurrentTime
  pure $
    RequestLog
      { logId
      , clientAddr = show sockAddr
      , path = unpack path'
      , loggedAt = logTime
      }

type RequestLog = RequestLogT Identity

deriving instance Show RequestLog

instance Beamable RequestLogT

instance Table RequestLogT where
  data PrimaryKey RequestLogT f = RequestLogId (Columnar f UUID)
    deriving (Generic, Beamable)
  primaryKey = RequestLogId . logId
