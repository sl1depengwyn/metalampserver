module Server.Main (main) where

import Universum
import Data.Aeson.Extended (FromJSON)
import qualified Data.Aeson.Extended as A
import qualified Database
import qualified Logger
import qualified Server

data Config = Config
  { cDatabase :: Database.Config,
    cLogger :: Logger.Config,
    cServer :: Server.Config
  }
  deriving (Show, Generic)

instance FromJSON Config where
  parseJSON = A.genericParseJSON A.customOptions

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
  let bot = cBot conf
  let logger = cLogger conf
  let plotter = cPlotter conf
  let toRun =
        case Bot.cHost bot of
          Bot.Tg _ -> Tg.run
  Logger.withHandle
    logger
    ( \hLogger ->
        Plotter.withHandle
          plotter
          ( \hPlotter ->
              Database.withHandle
                db
                hLogger
                ( \hDb -> Bot.withHandle bot hDb hLogger hPlotter toRun
                )
          )
    )
