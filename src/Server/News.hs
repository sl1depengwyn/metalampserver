{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Server.News where

import Data.Aeson.Extended (FromJSON)
import qualified Data.Aeson.Extended as A
import qualified Data.ByteString.Lazy as BSL (fromStrict)
import Data.Text (toLower)
import Data.Time.Calendar
import qualified Database as DB
import Database.Migration
import qualified Logger
import Servant
import Servant.API.ContentTypes
import Server
import Universum hiding (Handle)
import Universum.String.Reexport

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

instance FromHttpApiData DB.Sorting where
  parseQueryParam (toLower -> "date") = Right DB.Date
  parseQueryParam (toLower -> "author") = Right DB.Author
  parseQueryParam (toLower -> "cat") = Right DB.Cat
  parseQueryParam (toLower -> "images") = Right DB.Images
  parseQueryParam val = Left ("unknown value" <> val)

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
handleNews
  createdAt
  createdUntil
  createdSince
  authorName
  cat
  title
  text
  find
  sortBy = do
    dbh <- askDbh
    liftIO $ DB.getNews dbh (DB.NewsQueryParams {..})
