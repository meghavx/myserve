{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Auth (WithTokenAuth) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Pool
import Data.Proxy (Proxy (Proxy))
import Database.Beam
  ( all_
  , filter_
  , runSelectReturningOne
  , select
  , val_
  , (==.)
  )
import Database.Beam.Postgres (runBeamPostgres)
import Database.PostgreSQL.Simple (Connection)
import Database.Schema
import Network.Wai (requestHeaders)
import Servant
  ( err401
  , err403
  , parseHeader
  , (:>)
  )
import Servant.Server.Internal
  ( Delayed (..)
  , DelayedIO
  , HasContextEntry (..)
  , HasServer (..)
  , delayedFailFatal
  )

-- | A combinator that provides an auth token check before handler execution.
data WithTokenAuth

instance
  (HasServer api ctx, HasContextEntry ctx (Pool Connection))
  => HasServer (WithTokenAuth :> api) ctx
  where
  type ServerT (WithTokenAuth :> api) m = ServerT api m
  hoistServerWithContext Proxy = hoistServerWithContext (Proxy :: Proxy api)
  route Proxy context (Delayed captures' method' auth' accept' content' params' headers' body' server') =
    route
      (Proxy :: Proxy api)
      context
      Delayed
        { headersD = do
            headerResults <- headers'
            checkAuthToken (getContextEntry context)
            pure headerResults
        , capturesD = captures'
        , methodD = method'
        , authD = auth'
        , acceptD = accept'
        , contentD = content'
        , paramsD = params'
        , bodyD = body'
        , serverD = server'
        }
   where
    checkAuthToken :: Pool Connection -> DelayedIO ()
    checkAuthToken pool = do
      authMay <- asks (lookup authHeader . requestHeaders)
      case fmap parseHeader authMay of
        Nothing -> delayedFailFatal err401
        Just (Right token) -> do
          tokenMay <- withResource pool $ \conn ->
            liftIO $
              runBeamPostgres conn $
                runSelectReturningOne $
                  select $
                    filter_ (\t -> authToken t ==. val_ token) $
                      all_ (authTokens devDb)
          case tokenMay of
            Nothing -> delayedFailFatal err403
            Just _ -> pure ()
        Just (Left _) -> delayedFailFatal err401
    authHeader = "Authorization"
