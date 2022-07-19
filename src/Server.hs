{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Data.Aeson.Extended (FromJSON)
import qualified Data.Aeson.Extended as A
import qualified Data.ByteString.Lazy as BSL (fromStrict)
import Data.Time.Calendar
import qualified Database as DB
import Database.Migration
import qualified Logger
import Servant
import Servant.API.ContentTypes
import Universum hiding (Handle)

--import Server.News

newtype Config = Config {cPort :: Int} deriving (Show, Generic)

instance FromJSON Config where
  parseJSON = A.genericParseJSON (A.customOptionsWithDrop 1)

data Handle = Handle
  { hConfig :: Config,
    hDatabase :: DB.Handle,
    hLogger :: Logger.Handle
  }

withHandle ::
  Config ->
  DB.Handle ->
  Logger.Handle ->
  (Handle -> IO ()) ->
  IO ()
withHandle c db logger f = f Handle {hConfig = c, hDatabase = db, hLogger = logger}

type AppM = ReaderT Handle Handler

data WithCT = WithCT {header :: ByteString, content :: ByteString}

instance AllCTRender '[IMAGE] WithCT where
  handleAcceptH _ _ (WithCT h c) = Just (BSL.fromStrict h, BSL.fromStrict c)

data IMAGE deriving (Typeable)

instance MimeRender IMAGE ByteString where
  mimeRender _ = BSL.fromStrict

instance Accept IMAGE where
  contentType _ = ""

askLogger :: AppM Logger.Handle
askLogger = asks hLogger

askDbh :: AppM DB.Handle
askDbh = asks hDatabase
