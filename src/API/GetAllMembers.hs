{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module API.GetAllMembers (GetAllMembers, getAllMembers) where

import Auth (WithTokenAuth)
import qualified Data.List as L
import Database.Beam
  ( all_
  , runSelectReturningList
  , select
  )
import Database.Class (HasDb (..))
import Database.Schema
import Handler (Handler)
import Servant
  ( FromHttpApiData (parseQueryParam)
  , Get
  , JSON
  , QueryParam
  , (:>)
  )

data SortBy = Name | DateOfBirth

instance FromHttpApiData SortBy where
  parseQueryParam "name" = Right Name
  parseQueryParam "dateOfBirth" = Right DateOfBirth
  parseQueryParam got =
    Left $ "Invalid value for sortBy: Expect name or dateOfBirth, but got " <> got

type GetAllMembers =
  "v1"
    :> "members"
    :> QueryParam "sortBy" SortBy
    :> WithTokenAuth
    :> Get '[JSON] [Member]

-- | Get all members
getAllMembers
  :: Maybe SortBy
  -- ^ Defaults to sorting by name if 'Nothing'
  -> Handler [Member]
getAllMembers sortBy = do
  members' <- runDb $ runSelectReturningList $ select $ all_ (members devDb)
  pure $ sort sortBy members'
 where
  sort :: Maybe SortBy -> [Member] -> [Member]
  sort (Just DateOfBirth) members' = L.sortOn dateOfBirth members'
  sort _ members' = L.sortOn (\m -> firstName m <> lastName m) members'
