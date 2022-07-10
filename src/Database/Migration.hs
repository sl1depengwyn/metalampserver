{-# LANGUAGE OverloadedStrings #-}

module Database.Migration
  ( module Database.Migration.V001,
    migration,
    db,
    allowDestructive
  )
where

import Database.Beam (DatabaseSettings)
import Database.Beam.Migrate.Simple
    ( CheckedDatabaseSettings,
      evaluateDatabase,
      migrationStep,
      unCheckDatabase,
      MigrationSteps,
      defaultUpToDateHooks,
      BringUpToDateHooks(runIrreversibleHook) )
import Database.Beam.Migrate.Types
  ( CheckedDatabaseSettings,
    MigrationSteps,
    evaluateDatabase,
    migrationStep,
    unCheckDatabase,
  )
import Database.Beam.Postgres (Postgres)
import Database.Migration.V001 hiding (migration)
import qualified Database.Migration.V001 as V0001 (migration)
import Universum

migration :: MigrationSteps Postgres () (CheckedDatabaseSettings Postgres NewsDb)
migration = migrationStep "Initial commit" V0001.migration

db :: DatabaseSettings Postgres NewsDb
db = unCheckDatabase (evaluateDatabase migration)

allowDestructive :: (Monad m, MonadFail m) => BringUpToDateHooks m
allowDestructive =
  defaultUpToDateHooks
    { runIrreversibleHook = pure True
    }