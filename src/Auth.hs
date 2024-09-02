{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Auth
  ( WithTokenAuth
  , UserId
  , PostAuth (..)
  ) where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Proxy (Proxy (Proxy))
import Data.String (IsString)
import Data.Text (Text)
import Data.Time
  ( addUTCTime
  , getCurrentTime
  )
import Orville.PostgreSQL 
  ( ConnectionPool
  , runOrville
  , findEntity
  , deleteEntity
  )
import Database.Schema
  ( AuthToken (..)
  , authTokenTable
  )
import Network.Wai (requestHeaders)
import Servant
  ( err401
  , err403
  , parseHeader
  , (:>)
  )
import Servant.Server.Internal
  ( DelayedIO
  , HasContextEntry (..)
  , HasServer (..)
  , addAuthCheck
  , delayedFailFatal
  )

-- | What happens with the token after authentication.
data PostAuth
  = -- | Handy where token is no longer needed after authentication; e.g. logout
    DiscardToken
  | KeepToken

class KnownTokenDeleteStatus (postAuth :: PostAuth) where
  tokenCanBeDeleted :: Proxy postAuth -> Bool

instance KnownTokenDeleteStatus DiscardToken where
  tokenCanBeDeleted _ = True

instance KnownTokenDeleteStatus KeepToken where
  tokenCanBeDeleted _ = False

-- | Check auth token before handler execution.
data WithTokenAuth (postAuth :: PostAuth)

type UserId = Text

authHeader :: IsString a => a
authHeader = "Authorization"

instance
  ( HasServer api ctx
  , HasContextEntry ctx ConnectionPool
  , HasContextEntry ctx Integer
  , KnownTokenDeleteStatus postAuth
  )
  => HasServer (WithTokenAuth postAuth :> api) ctx
  where
  type ServerT (WithTokenAuth postAuth :> api) m =
    UserId -> ServerT api m
  hoistServerWithContext _ ctx nt server =
    hoistServerWithContext (Proxy :: Proxy api) ctx nt . server
  route _ context server = do
    route (Proxy @api) context $
      server
        `addAuthCheck` ( checkAuthToken
                          (getContextEntry context) -- connection pool
                          (getContextEntry context) -- auth token timeout
                       )
   where
    checkAuthToken
      :: ConnectionPool -> Integer -> DelayedIO UserId
    checkAuthToken pool authTokenTimeoutSeconds = do
      authMay <- asks (lookup authHeader . requestHeaders)
      case fmap parseHeader authMay of
        Nothing -> 
          delayedFailFatal err401
        Just (Left _) -> 
          delayedFailFatal err401
        Just (Right gotToken) -> do
          tokenMay <- liftIO $ runOrville pool $
            findEntity authTokenTable gotToken
          case tokenMay of
            Nothing -> 
              delayedFailFatal err403
            Just t -> do
              now <- liftIO getCurrentTime
              let (createdBy', createdAt') = (createdBy t, createdAt t)
                  tokenExpired = now > addUTCTime (fromIntegral authTokenTimeoutSeconds) createdAt'
              when (tokenExpired || tokenCanBeDeleted (Proxy @postAuth)) $
                liftIO $ runOrville pool $
                  deleteEntity authTokenTable gotToken
              if tokenExpired
                then delayedFailFatal err403
                else pure createdBy'