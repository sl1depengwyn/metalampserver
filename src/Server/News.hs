{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Server.News where

import Data.Aeson.Extended (FromJSON)
import qualified Data.Aeson.Extended as A
import qualified Data.ByteString.Lazy as BSL (fromStrict)
import Data.Time.Calendar
import Database.Migration
import qualified Database.News as DB
import qualified Logger
import Servant
import Servant.API.ContentTypes
import Server
import Universum hiding (Handle)

type NewsApi =
  "news" :> QueryParam "created_at" Day
    :> QueryParam "created_until" Day
    :> QueryParam "created_since" Day
    :> QueryParam "author" Text
    :> QueryParam "category" Int
    :> QueryParam "title" Text
    :> QueryParam "text" Text
    :> QueryParam "find" Text -- looking for news with this string in either news text, title or category
    :> QueryParam "sort_by" DB.Sorting
    :> Get '[JSON] [News]

handleNews ::
  Maybe Day ->
  Maybe Day ->
  Maybe Day ->
  Maybe Text ->
  Maybe Int ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe DB.Sorting ->
  AppM [News]
handleNews = undefined

api :: Proxy NewsApi
api = Proxy

server :: ServerT NewsApi AppM
server = handleNews
