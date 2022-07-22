{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module Server.Users where

import Data.Aeson.Extended (FromJSON, ToJSON)
import qualified Data.Aeson.Extended as A
import qualified Data.Pool as Pool
import Data.Time.Calendar
import Database.Beam
import Database.Beam.Backend
import Database.Beam.Migrate
import Database.Beam.Migrate.Simple
import Database.Beam.Postgres
import qualified Database.Beam.Postgres.Migrate as PG
import Database.Beam.Query.CTE (QAnyScope)
import Database.Beam.Query.Internal
import Database.Migration
import qualified Database.PostgreSQL.Simple as PGS
import Lens.Micro
import qualified Logger
import Universum hiding (Handle, sortBy)

data UserToReturn = UserToReturn
  { utrName :: Text,
    utrDateOfRegistration :: Day
  }
  deriving (Show, Generic)

instance FromJSON UserToReturn where
  parseJSON = A.genericParseJSON (A.customOptionsWithDrop 3)

instance ToJSON UserToReturn where
  toJSON = A.genericToJSON (A.customOptionsWithDrop 3)

userToReturn :: User -> UserToReturn
userToReturn u = UserToReturn {utrName = u ^. userName, utrDateOfRegistration = u ^. userRegDate}