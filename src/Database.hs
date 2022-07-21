{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Database
  ( module Database.News,
    withHandle,
    Handle (..),
    Config,
    getNews,
    getUser,
  )
where

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
import Database.News
import qualified Database.PostgreSQL.Simple as PGS
import Database.Users
import Lens.Micro
import qualified Logger
import Universum hiding (Handle)

newtype Config = Config
  { cConnectionString :: Text
  }
  deriving (Show, Generic)

instance FromJSON Config where
  parseJSON = A.genericParseJSON (A.customOptionsWithDrop 1)

data Handle = Handle
  { hConfig :: Config,
    hLogger :: Logger.Handle,
    hPool :: Pool.Pool PGS.Connection
  }

withHandle :: Config -> Logger.Handle -> (Handle -> IO a) -> IO a
withHandle conf lh f = do
  let connString = cConnectionString conf
  pool <- Pool.createPool (PGS.connectPostgreSQL (encodeUtf8 connString)) PGS.close 1 10 4
  let h = Handle {hConfig = conf, hLogger = lh, hPool = pool}
  Pool.withResource pool (migrateDB lh)
  res <- f h
  Pool.destroyAllResources pool
  return res

migrateDB :: Logger.Handle -> Connection -> IO (Maybe (CheckedDatabaseSettings Postgres NewsDb))
migrateDB h conn =
  runBeamPostgresDebug (Logger.debug h) conn $
    bringUpToDateWithHooks
      allowDestructive
      PG.migrationBackend
      migration

runQuery :: Handle -> Pg b -> IO b
runQuery h q = Pool.withResource (hPool h) $ \conn -> runBeamPostgresDebug (Logger.debug (hLogger h)) conn q

getNews :: Handle -> Integer -> Integer -> NewsQueryParams -> IO [(News, User, Cat)]
getNews h limit offset params = runQuery h (paginated getNews' limit offset params)

getUser :: Handle -> Text -> IO (Maybe User)
getUser h login = runQuery h (runSelectReturningOne (select (getUser' login)))

paginated f limit offset = runSelectReturningList . select . limit_ limit . offset_ offset . f