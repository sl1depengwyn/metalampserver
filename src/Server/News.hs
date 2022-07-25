{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Server.News where

import Data.Aeson.Extended (FromJSON, ToJSON)
import qualified Data.Aeson.Extended as A
import qualified Data.ByteString.Lazy as BSL (fromStrict)
import Data.Text (toLower)
import Data.Time.Calendar
import qualified Database as Db
import Database.Beam.Backend (unSerial)
import Database.Migration
import qualified Logger
import Servant
import Servant.API.ContentTypes
import Server
import Server.Cats
import Server.Images
import Server.Users
import Universum hiding (Handle)
import Universum.String.Reexport
import qualified Database as Db

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
    :> QueryParam "sort_order" Db.SortOrder
    :> QueryParam "limit" Integer
    :> QueryParam "offset" Integer
    :> Get '[JSON] [PostToReturn]

instance FromHttpApiData Db.Sorting where
  parseQueryParam (toLower -> "date") = Right Db.ByDate
  parseQueryParam (toLower -> "author") = Right Db.ByAuthor
  parseQueryParam (toLower -> "cat") = Right Db.ByCat
  parseQueryParam (toLower -> "images") = Right Db.ByNoImages
  parseQueryParam val = Left ("unknown value for sorting " <> val)

instance FromHttpApiData Db.SortOrder where
  parseQueryParam (toLower -> "asc") = Right Db.Asc
  parseQueryParam (toLower -> "desc") = Right Db.Desc
  parseQueryParam val = Left ("unknown value for sorting order" <> val)

data PostToReturn = PostToReturn
  { ptrId :: Int32,
    ptrTitle :: Text,
    ptrDateOfCreation :: Day,
    ptrCreator :: UserToReturn,
    ptrCat :: Maybe CatToReturn,
    ptrText :: Text,
    ptrImages :: [ImageToReturn],
    ptrIsPublished :: Bool
  }
  deriving (Show, Generic)

instance FromJSON PostToReturn where
  parseJSON = A.genericParseJSON (A.customOptionsWithDrop 3)

instance ToJSON PostToReturn where
  toJSON = A.genericToJSON (A.customOptionsWithDrop 3)

postToReturn :: News -> User -> [Maybe Cat] -> [Int32] -> PostToReturn
postToReturn news user cat iIds =
  PostToReturn
    { ptrId = unSerial (news ^. newsId),
      ptrTitle = news ^. newsTitle,
      ptrDateOfCreation = news ^. newsCreatedAt,
      ptrCreator = userToReturn user,
      ptrCat = catToReturn cat,
      ptrText = news ^. newsText,
      ptrImages = map imageToReturn iIds,
      ptrIsPublished = news ^. isNewsPublished
    }

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
  Maybe Db.SortOrder ->
  Maybe Integer ->
  Maybe Integer ->
  AppM [PostToReturn]
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
  sortOrder
  limit'
  offset' = do
    dbh <- askDbh
    def <- askLimit
    let limit = limitFromMaybe def limit'
        offset = offsetFromMaybe offset'
    news <- liftIO $ Db.getNews dbh limit offset (Db.NewsQueryParams {..})
    mapM
      ( \(n, a, _) -> do
          cats <- liftIO $ Db.getCategory dbh (n ^. newsCat)
          iIds <- liftIO $ Db.getNewsImages dbh (n ^. newsId)
          pure (postToReturn n a cats iIds)
      )
      news