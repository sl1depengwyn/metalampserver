{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Server.Images where

import Data.Aeson.Extended (FromJSON, ToJSON)
import qualified Data.Aeson.Extended as A
import qualified Data.Pool as Pool
import Data.Time.Calendar
import Database.Beam
import Database.Beam.Backend
import Database.Beam.Migrate
import Database.Beam.Migrate.Simple
import Database.Beam.Postgres
import Database.Beam.Postgres.Full (lateral_)
import qualified Database.Beam.Postgres.Migrate as Pg
import Database.Beam.Query.CTE (QAnyScope)
import Database.Beam.Query.Internal
import Database.Cats
import Database.Migration
import qualified Database.PostgreSQL.Simple as Pgs
import Database.Users
import Lens.Micro
import qualified Logger
import Universum hiding (Handle, sortBy)

newtype ImageToReturn = ImageToReturn
  { itrUri :: Text
  }
  deriving (Show, Generic)

instance FromJSON ImageToReturn where
  parseJSON = A.genericParseJSON (A.customOptionsWithDrop 3)

instance ToJSON ImageToReturn where
  toJSON = A.genericToJSON (A.customOptionsWithDrop 3)

imageToReturn ::
  Int32 ->
  -- | imageId
  ImageToReturn
imageToReturn iId = ImageToReturn {itrUri = "images/get/" <> show iId}