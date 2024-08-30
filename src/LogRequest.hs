{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module LogRequest (LogRequest, LogMode (..)) where

import Data.Aeson (encode)
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Lazy.Char8 (toStrict)
import Data.Foldable (for_)
import Data.Functor ((<&>))
import Data.List (nub)
import Data.Pool (Pool, withResource)
import Data.Proxy (Proxy (Proxy))
import Database.Beam (insert, insertValues, runInsert)
import Database.Beam.Postgres (runBeamPostgres)
import Database.PostgreSQL.Simple (Connection)
import Database.Schema (devDb, requestLogs)
import Database.Schema.RequestLog (RequestLog, mkRequestLog)
import Network.Wai (rawPathInfo, remoteHost)
import Servant ((:>))
import Servant.Server.Internal (HasContextEntry (..), HasServer (..))

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
  , HasContextEntry ctx (Pool Connection)
  , GetLogModes modes
  )
  => HasServer (LogRequest modes :> api) ctx
  where
  type ServerT (LogRequest modes :> api) m = ServerT api m
  hoistServerWithContext Proxy = hoistServerWithContext (Proxy :: Proxy api)
  route Proxy context delayed =
    route (Proxy :: Proxy api) context delayed <&> \app req respK -> do
      requestLog <-
        mkRequestLog (remoteHost req) (rawPathInfo req)
      let pool :: Pool Connection = getContextEntry context
      let logModes = nub $ getLogModes (Proxy :: Proxy modes)
      for_ logModes (logger pool requestLog)
      app req respK
   where
    logger
      :: Pool Connection -> RequestLog -> LogMode -> IO ()
    logger _ requestLog StdoutLog = putStrLn $ "Request: " <> (unpack $ toStrict $ encode requestLog)
    logger pool requestLog DbLog = withResource pool $ \conn ->
      runBeamPostgres conn $
        runInsert $
          insert (requestLogs devDb) $
            insertValues [requestLog]
