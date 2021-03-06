{-# LANGUAGE OverloadedStrings #-}

module Database.Migration
  ( module Database.Migration.V003,
    migration,
    db,
    allowDestructive,
  )
where

import Control.Arrow ((>>>))
import Database.Beam (DatabaseSettings)
import Database.Beam.Migrate.Simple
  ( BringUpToDateHooks (runIrreversibleHook),
    CheckedDatabaseSettings,
    MigrationSteps,
    defaultUpToDateHooks,
    evaluateDatabase,
    migrationStep,
    unCheckDatabase,
  )
import Database.Beam.Migrate.Types
  ( CheckedDatabaseSettings,
    MigrationSteps,
    evaluateDatabase,
    migrationStep,
    unCheckDatabase,
  )
import Database.Beam.Postgres (Postgres)
import qualified Database.Migration.V001 as V001 (migration)
import qualified Database.Migration.V002 as V002 (migration)
import Database.Migration.V003 hiding (migration)
import qualified Database.Migration.V003 as V003 (migration)
import Universum

migration :: MigrationSteps Postgres () (CheckedDatabaseSettings Postgres NewsDb)
migration =
  migrationStep "Initial commit" V001.migration
    >>> migrationStep "Make Category parent field nullable" V002.migration
    >>> migrationStep "Add noPictures column to news" V003.migration

db :: DatabaseSettings Postgres NewsDb
db = unCheckDatabase (evaluateDatabase migration)

allowDestructive :: (Monad m, MonadFail m) => BringUpToDateHooks m
allowDestructive =
  defaultUpToDateHooks
    { runIrreversibleHook = pure True
    }
