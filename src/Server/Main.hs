{-# LANGUAGE DeriveGeneric #-}

module Server.Main (main) where

import Data.Aeson.Extended (FromJSON)
import qualified Data.Aeson.Extended as A
import qualified Data.Yaml as Yaml
import qualified Database
import qualified Logger
import qualified Server
import qualified Server.Api as Api
import System.Environment
import Universum

data Config = Config
  { cDatabase :: Database.Config
  , cLogger :: Logger.Config
  , cServer :: Server.Config
  }
  deriving (Show, Generic)

instance FromJSON Config where
  parseJSON = A.genericParseJSON (A.customOptionsWithDrop 1)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [configPath] -> run configPath
    _ -> run "config.yaml"

run :: FilePath -> IO ()
run path = do
  errOrConfig <- Yaml.decodeFileEither path
  conf <- either (fail . show) pure errOrConfig
  let db = cDatabase conf
      logger = cLogger conf
      server = cServer conf
  Logger.withHandle
    logger
    ( \logh ->
        Database.withHandle
          db
          logh
          ( \dbh ->
              Server.withHandle server dbh logh Api.startApp
          )
    )
