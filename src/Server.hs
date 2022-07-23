{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Data.Aeson.Extended (FromJSON)
import qualified Data.Aeson.Extended as A
import qualified Data.ByteString.Lazy as BSL (fromStrict)
import Data.Time.Calendar
import qualified Database as Db
import Database.Migration
import GHC.Base (Symbol)
import qualified Logger
import Servant
import Servant.API.ContentTypes
import Universum hiding (Handle)

--import Server.News

data Config = Config
  { cPort :: Int,
    cLimit :: Maybe Integer,
    cHashCost :: Maybe Int
  }
  deriving (Show, Generic)

instance FromJSON Config where
  parseJSON = A.genericParseJSON (A.customOptionsWithDrop 1)

data Handle = Handle
  { hConfig :: Config,
    hDatabase :: Db.Handle,
    hLogger :: Logger.Handle
  }

withHandle ::
  Config ->
  Db.Handle ->
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

askMbValue :: a -> (Handle -> Maybe a) -> AppM a
askMbValue def f = fromMaybe def <$> asks f

askLogger :: AppM Logger.Handle
askLogger = asks hLogger

askDbh :: AppM Db.Handle
askDbh = asks hDatabase

askLimit :: AppM Integer
askLimit = askMbValue 20 (cLimit . hConfig)

askCost :: AppM Int
askCost = askMbValue 12 (cHashCost . hConfig)

limitFromMaybe :: Integer -> Maybe Integer -> Integer
limitFromMaybe def limit
  | defOrLimit > def = def
  | defOrLimit < 0 = 0
  | otherwise = defOrLimit
  where
    defOrLimit = fromMaybe def limit

offsetFromMaybe :: Maybe Integer -> Integer
offsetFromMaybe offset
  | defOrOffset < 0 = 0
  | otherwise = defOrOffset
  where
    defOrOffset = fromMaybe 0 offset