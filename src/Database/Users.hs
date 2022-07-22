{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Users where

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

getUser' :: Text -> Q Postgres NewsDb s (UserT (QExpr Postgres s))
getUser' login = filter_ (\user -> user ^. userLogin ==. val_ login) (all_ (db ^. nUsers))
