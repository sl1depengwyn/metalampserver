{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Database.Images where

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


getImageIdsByNews :: Int32 -> Q Postgres NewsDb s (QExpr Postgres s (SqlSerial Int32))
getImageIdsByNews newsId = do
    imgId <- filter_ (\itn -> itn ^. itnNewsId ==. val_ (SqlSerial newsId)) (all_ (db ^. nImagesToNews))
    pure (imgId ^. itnImageId)

getImageById = undefined