{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Logger
  ( Verbosity (..),
    Config (..),
    Handle,
    withHandle,
    debug,
    info,
    warning,
    error,
    debug',
    info',
    warning',
    error',
  )
where

import Data.Aeson.Extended (FromJSON)
import qualified Data.Aeson.Extended as A
import qualified System.Log.FastLogger as FL
import Universum hiding (Handle, error)

data Verbosity
  = Debug
  | Info
  | Warning
  | Error
  deriving (Eq, Ord, Show)

instance FromJSON Verbosity where
  parseJSON = A.withText "FromJSON Fugacious.Logger.Verbosity" $ \t ->
    case t of
      "debug" -> pure Debug
      "info" -> pure Info
      "warning" -> pure Warning
      "error" -> pure Error
      _ -> fail $ "Unknown verbosity: " ++ toString t

data Config = Config
  { cPath :: Maybe FilePath,
    cVerbosity :: Maybe Verbosity
  }
  deriving (Show, Generic)

instance A.FromJSON Config where
  parseJSON = A.genericParseJSON (A.customOptionsWithDrop 1)

data Handle = Handle
  { hConfig :: Config,
    hLoggerSet :: FL.LoggerSet
  }

withHandle :: Config -> (Handle -> IO a) -> IO a
withHandle config f =
  bracket
    ( case cPath config of
        Nothing -> FL.newStderrLoggerSet FL.defaultBufSize
        Just "-" -> FL.newStderrLoggerSet FL.defaultBufSize
        Just path -> FL.newFileLoggerSet FL.defaultBufSize path
    )
    FL.rmLoggerSet
    (\l -> f Handle {hConfig = config, hLoggerSet = l})

log :: FL.ToLogStr s => Handle -> Verbosity -> s -> IO ()
log Handle {..} v x
  | v >= verbosity = FL.pushLogStrLn hLoggerSet $ FL.toLogStr x
  | otherwise = pass
  where
    verbosity = fromMaybe Debug (cVerbosity hConfig)

debug, info, warning, error :: FL.ToLogStr str => Handle -> str -> IO ()
debug h = log h Debug
info h = log h Info
warning h = log h Warning
error h = log h Error

debug', info', warning', error' :: Handle -> Text -> IO ()
debug' = debug
info' = info
warning' = warning
error' = error
