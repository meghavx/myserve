{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API.AddMember (AddMember, addMember) where

import Auth (WithTokenAuth)
import Database.Beam
  ( insert
  , insertValues
  , runInsert
  )
import Database.Class (HasDb (runDb))
import Database.Schema
import Handler (Handler)
import Servant
  ( JSON
  , NoContent (NoContent)
  , Post
  , ReqBody
  , (:>)
  )

type AddMember =
  "v1"
    :> "member"
    :> ReqBody '[JSON] Member
    :> WithTokenAuth
    :> Post '[JSON] NoContent

-- | Add a member to the member database
addMember :: Member -> Handler NoContent
addMember member = do
  runDb $ runInsert $ insert (members devDb) $ insertValues [member]
  pure NoContent
