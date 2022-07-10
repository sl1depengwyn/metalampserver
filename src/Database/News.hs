{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Database.News where

import Data.Aeson.Extended (FromJSON)
import qualified Data.Aeson.Extended as A
import qualified Data.Pool as Pool
import Data.Time.Calendar
import Database.Beam
import Database.Beam.Migrate
import Database.Beam.Migrate.Simple
import Database.Beam.Postgres
import qualified Database.Beam.Postgres.Migrate as PG
import Database.Migration
import qualified Database.PostgreSQL.Simple as PGS
import Lens.Micro
import qualified Logger
import Universum hiding (Handle)

data Sorting = Date | Author | Cat | Images

data NewsQueryParams = NewsQueryParams
  { createdAt :: Maybe Day,
    createdUntil :: Maybe Day,
    createdSince :: Maybe Day,
    author :: Maybe Text,
    cat :: Maybe Int,
    title :: Maybe Text,
    text :: Maybe Text,
    find :: Maybe Text,
    sortBy :: Maybe Sorting
  }


getNews :: NewsQueryParams -> Q Postgres NewsDb s b
getNews NewsQueryParams{..} = do
  news <- all_ $ db ^. nNews
  let dateAt = maybe (val_ True) () createdAt
  undefined