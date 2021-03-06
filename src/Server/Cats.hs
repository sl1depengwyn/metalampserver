{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Server.Cats where

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
import Database.Users
import Lens.Micro
import qualified Logger
import Universum hiding (Handle, sortBy)

data CatToReturn = CatToReturn
  { ctrId :: Int32,
    ctrName :: Text,
    ctrParent :: Maybe CatToReturn
  }
  deriving (Show, Generic)

instance FromJSON CatToReturn where
  parseJSON = A.genericParseJSON (A.customOptionsWithDrop 3)

instance ToJSON CatToReturn where
  toJSON = A.genericToJSON (A.customOptionsWithDrop 3)

catToReturn ::
  [Maybe Cat] ->
  -- | list of the following schema: Just child : Just parent : Just grandparent : ... : Nothing : []
  --   (basically it's what sql returns)
  Maybe CatToReturn
catToReturn ((Just c) : cs) =
  Just
    CatToReturn
      { ctrId = unSerial (c ^. catId),
        ctrName = c ^. catName,
        ctrParent = catToReturn cs
      }
catToReturn _ = Nothing