{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API.GetMember (GetMember, getMember) where

import Auth (WithTokenAuth)
import Data.Text (Text)
import Database.Beam
  ( all_
  , filter_
  , runSelectReturningOne
  , select
  , val_
  , (==.)
  )
import Database.Class (HasDb (runDb))
import Database.Schema
import Handler (Handler)
import Servant
  ( Capture
  , Get
  , JSON
  , (:>)
  )

type GetMember =
  "v1"
    :> "member"
    :> Capture "email" Text
    :> WithTokenAuth
    :> Get '[JSON] (Maybe Member)

-- | Get member given email ID
getMember :: Text -> Handler (Maybe Member)
getMember email' =
  runDb $
    runSelectReturningOne $
      select $
        filter_ (\member -> email member ==. val_ email') $
          all_ (members devDb)
