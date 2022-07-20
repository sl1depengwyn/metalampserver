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
import qualified Database as Db
import Database.Migration
import qualified Logger
import Servant
import Servant.API.ContentTypes
import Server
import Universum hiding (Handle)
import Universum.String.Reexport

type NewsApi =
  "news"
    :> QueryParam "created_at" Day
    :> QueryParam "created_until" Day
    :> QueryParam "created_since" Day
    :> QueryParam "author" Text
    :> QueryParam "category" Int
    :> QueryParam "title" Text
    :> QueryParam "text" Text
    :> QueryParam "find" Text -- looking for news with this string in either news text, title or category
    :> QueryParam "sort_by" Db.Sorting
    :> QueryParam "limit" Integer
    :> QueryParam "offset" Integer
    :> Get '[JSON] [News]

instance FromHttpApiData Db.Sorting where
  parseQueryParam (toLower -> "date") = Right Db.Date
  parseQueryParam (toLower -> "author") = Right Db.Author
  parseQueryParam (toLower -> "cat") = Right Db.Cat
  parseQueryParam (toLower -> "images") = Right Db.Images
  parseQueryParam val = Left ("unknown value " <> val)

handleNews ::
  Maybe Day ->
  Maybe Day ->
  Maybe Day ->
  Maybe Text ->
  Maybe Int ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Db.Sorting ->
  Maybe Integer ->
  Maybe Integer ->
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
  sortBy
  limit'
  offset' = do
    dbh <- askDbh
    def <- askLimit
    let limit = limitFromMaybe def limit'
        offset = offsetFromMaybe offset'
    liftIO $ Db.getNews dbh limit offset (Db.NewsQueryParams {..})