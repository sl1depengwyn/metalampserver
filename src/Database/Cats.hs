{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Database.Cats where

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
import Universum hiding (Handle, With, sortBy)

getCategories :: Q Postgres NewsDb s (CatT (QExpr Postgres s))
getCategories = all_ (db ^. nCats)

getCategory' :: SqlSerial Int32 -> With Postgres NewsDb (Q Postgres NewsDb s (CatT (Nullable (QExpr Postgres s))))
getCategory' cId = do
  rec fib <-
        selecting
          ( ( do -- base case
                cat <- filter_ (\c -> c ^. catId ==. val_ cId) (all_ (db ^. nCats))
                pure (just_ cat)
            )
              `union_` ( do -- recursive step
                           mbCat <- reuse fib
                           leftJoin_ (all_ (db ^. nCats)) (\parent -> maybe_ (val_ False) (\child -> just_ (parent ^. catId) ==. child ^. catParent) mbCat)
                       )
          )
  pure (reuse fib)