{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module LogRequest (LogRequest, LogMode (..)) where

import Data.Aeson (encode)
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Lazy.Char8 (toStrict)
import Data.Foldable (for_)
import Data.Functor ((<&>))
import Data.List (nub)
import Data.Proxy (Proxy (Proxy))
import Orville.PostgreSQL 
  ( ConnectionPool
  , runOrville
  , findEntity
  , insertEntity
  )
import Database.Schema
  ( requestLogTable
  , RequestLog
  , mkRequestLog
  , authTokenTable
  , AuthToken (createdBy)
  )
import Network.Wai
  ( requestHeaders
  , rawPathInfo
  , remoteHost
  , requestMethod
  )
import Servant ((:>))
import Servant.Server.Internal (HasContextEntry (..), HasServer (..))
import Data.UUID (UUID, fromASCIIBytes, nil)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.ByteString as BS
import Network.HTTP.Types.Header (RequestHeaders, HeaderName)

-- | Log an incoming request
data LogRequest (modes :: [LogMode])

-- | Possible logging modes
data LogMode
  = -- | Logs to stdout
    StdoutLog
  | -- | Logs to the request_log database table (not implemented)
    DbLog
  deriving (Eq, Show)

class KnownLogMode mode where
  modeVal :: Proxy mode -> LogMode

instance KnownLogMode StdoutLog where
  modeVal Proxy = StdoutLog

instance KnownLogMode DbLog where
  modeVal Proxy = DbLog

class GetLogModes modes where
  getLogModes :: Proxy modes -> [LogMode]

instance GetLogModes '[] where
  getLogModes Proxy = []

instance (KnownLogMode mode, GetLogModes modes) => GetLogModes (mode : modes) where
  getLogModes Proxy = modeVal (Proxy @mode) : getLogModes (Proxy @modes)

instance
  ( HasServer api ctx
  , HasContextEntry ctx ConnectionPool
  , GetLogModes modes
  )
  => HasServer (LogRequest modes :> api) ctx
  where
  type ServerT (LogRequest modes :> api) m = 
    ServerT api m
  hoistServerWithContext Proxy = 
    hoistServerWithContext (Proxy @api)
  route Proxy context delayed =
    route (Proxy @api) context delayed <&> \app req respK -> do
      let pool :: ConnectionPool = getContextEntry context
          logModes = nub $ getLogModes (Proxy @modes)
          reqHeaders = requestHeaders req
          token = authToken reqHeaders
      mAuthRecord <- runOrville pool $ 
        findEntity authTokenTable token
      let userId = fromMaybe T.empty $ createdBy <$> mAuthRecord 
      requestLog <-
        mkRequestLog
          (requestMethod req)
          (rawPathInfo req)
          (remoteHost req)
          userId
          reqHeaders
      for_ logModes (logger pool requestLog)
      app req respK
   where 
    authToken :: RequestHeaders -> UUID
    authToken = 
      let lookUpToken = fromMaybe BS.empty . lookup ("Authorization" :: HeaderName)
          toUUID = fromMaybe nil . fromASCIIBytes
      in toUUID . lookUpToken

    logger :: ConnectionPool -> RequestLog -> LogMode -> IO ()
    logger _ requestLog StdoutLog = 
      putStrLn $ "Request: " <> (unpack $ toStrict $ encode requestLog)
    logger pool requestLog DbLog =
      runOrville pool $ 
        insertEntity requestLogTable requestLog