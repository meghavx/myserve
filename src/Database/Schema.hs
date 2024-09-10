module Database.Schema
  ( module X
  , migrateSchema
  ) where

import Database.Schema.User as X 
  ( User (..)
  , userTable
  )
import Database.Schema.AuthToken as X 
  ( AuthToken (..)
  , authTokenTable
  , createdByField
  )
import Database.Schema.RequestLog as X
  ( RequestLog (..)
  , requestLogTable
  , mkRequestLog
  )

import qualified Orville.PostgreSQL as O
import qualified Orville.PostgreSQL.AutoMigration as AutoMigration

-- Function to perform schema migration
migrateSchema :: O.Orville ()
migrateSchema = 
  AutoMigration.autoMigrateSchema 
    AutoMigration.defaultOptions
    [ AutoMigration.SchemaTable userTable
    , AutoMigration.SchemaTable authTokenTable
    , AutoMigration.SchemaTable requestLogTable
    ]
