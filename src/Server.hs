{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Server
  ( Api
  , server
  ) where

import API.AddMember (AddMember, addMember)
import API.GetAllMembers (GetAllMembers, getAllMembers)
import API.GetMember (GetMember, getMember)
import Data.Pool
import Data.Proxy (Proxy (Proxy))
import Database.PostgreSQL.Simple (Connection)
import Handler (toServantHandler)
import Servant
  ( Server
  , (:<|>) ((:<|>))
  )
import Servant.Server.Internal
  ( HasServer (hoistServerWithContext)
  )

type Api = GetAllMembers :<|> GetMember :<|> AddMember

server :: Pool Connection -> Server Api
server pool =
  hoistServerWithContext
    (Proxy :: Proxy Api)
    (Proxy :: Proxy '[Pool Connection])
    (toServantHandler pool)
    handlers
 where
  handlers = getAllMembers :<|> getMember :<|> addMember
