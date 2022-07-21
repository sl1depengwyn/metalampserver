{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Database.News where

import Data.Aeson.Extended (FromJSON, ToJSON)
import qualified Data.Aeson.Extended as A
import qualified Data.Pool as Pool
import Data.Time.Calendar
import Database.Beam
import Database.Beam.Backend
import Database.Beam.Migrate
import Database.Beam.Migrate.Simple
import Database.Beam.Postgres
import Database.Beam.Postgres.Full (lateral_)
import qualified Database.Beam.Postgres.Migrate as Pg
import Database.Beam.Query.CTE (QAnyScope)
import Database.Beam.Query.Internal
import Database.Cats
import Database.Migration
import qualified Database.PostgreSQL.Simple as Pgs
import Database.Users
import Lens.Micro
import qualified Logger
import Universum hiding (Handle, sortBy)

data Sorting
  = ByDate
  | ByAuthor
  | ByCat
  | ByNoImages

data SortOrder
  = Asc
  | Desc

data NewsQueryParams = NewsQueryParams
  { createdAt :: Maybe Day,
    createdUntil :: Maybe Day,
    createdSince :: Maybe Day,
    authorName :: Maybe Text,
    cat :: Maybe Int,
    title :: Maybe Text,
    text :: Maybe Text,
    find :: Maybe Text,
    sortBy :: Maybe Sorting,
    sortOrder :: Maybe SortOrder
  }

data PostToReturn = PostToReturn
  { ptrId :: Int32,
    ptrTitle :: Text,
    ptrDateOfCreation :: Day,
    ptrCreator :: UserToReturn,
    ptrCat :: CatToReturn,
    ptrText :: Text,
    ptrIsPublished :: Bool
  }
  deriving (Show, Generic)

instance FromJSON PostToReturn where
  parseJSON = A.genericParseJSON (A.customOptionsWithDrop 3)

instance ToJSON PostToReturn where
  toJSON = A.genericToJSON (A.customOptionsWithDrop 3)

postToReturn :: News -> User -> CatToReturn -> PostToReturn
postToReturn news user cat =
  PostToReturn
    { ptrId = unSerial (news ^. newsId),
      ptrTitle = news ^. newsTitle,
      ptrDateOfCreation = news ^. newsCreatedAt,
      ptrCreator = userToReturn user,
      ptrCat = cat,
      ptrText = news ^. newsText,
      ptrIsPublished = news ^. isNewsPublished
    }

toTextEntry :: (SqlValable a, HaskellLiteralForQExpr a ~ Text) => Text -> a
toTextEntry txt = val_ ("%" <> txt <> "%")

newsImages :: Q Postgres NewsDb s (NewsT (QExpr Postgres s), ImageT (QExpr Postgres s))
newsImages = manyToMany_ (db ^. nImagesToNews) _itnNewsId _itnImageId (all_ (db ^. nNews)) (all_ (db ^. nImages))

queryToWhere :: NewsQueryParams -> (NewsT (QExpr Postgres scope), a) -> QExpr Postgres scope Bool
queryToWhere NewsQueryParams {..} (news, _) =
  case mconcat [dateAt, dateUntil, dateSince, catId, titleText, textEntry] of
    x : xs -> foldr1 (&&.) (x :| xs)
    _ -> val_ True
  where
    dateAt = maybe [] (\date -> [val_ date ==. news ^. newsCreatedAt]) createdAt
    dateUntil = maybe [] (\date -> [val_ date >=. news ^. newsCreatedAt]) createdUntil
    dateSince = maybe [] (\date -> [val_ date <=. news ^. newsCreatedAt]) createdSince
    catId = maybe [] (\(fromIntegral -> ci) -> [val_ ci ==. news ^. newsCat]) cat
    titleText = maybe [] (\(toTextEntry -> t) -> [news ^. newsTitle `like_` t]) title
    textEntry = maybe [] (\(toTextEntry -> t) -> [news ^. newsText `like_` t]) text

getNews' :: NewsQueryParams -> Q Postgres NewsDb scope (NewsT (QExpr Postgres scope), UserT (QExpr Postgres scope), CatT (QExpr Postgres scope))
getNews' params@NewsQueryParams {..} = maybe query ordered sortBy
  where
    query :: Q Postgres NewsDb s (NewsT (QExpr Postgres s), UserT (QExpr Postgres s), CatT (QExpr Postgres s))
    query = do
      (news, image) <- filter_ (queryToWhere params) newsImages
      author <- related_ (db ^. nUsers) (_nCreator news)
      cat <- related_ (db ^. nCats) (_nCat news)

      whenJust text $ \(toTextEntry -> text) ->
        guard_
          ( (news ^. newsText `like_` text)
              ||. (author ^. userName `like_` text)
              ||. (cat ^. catName `like_` text)
          )

      whenJust authorName $ \usernameToFind ->
        guard_ (author ^. userName `like_` val_ usernameToFind)

      pure (news, author, cat)
    order :: QExpr Postgres s a -> QOrd Postgres s a
    order = case sortOrder of
      Just Desc -> desc_
      _ -> asc_

    ordered s = case s of
      ByDate -> orderBy_ (\(n, a, c) -> order (n ^. newsCreatedAt)) query
      ByAuthor -> orderBy_ (\(n, a, c) -> order (a ^. userName)) query
      ByCat -> orderBy_ (\(n, a, c) -> order (c ^. catName)) query
      ByNoImages -> orderBy_ (\(n, a, c) -> order (n ^. newsCreatedAt)) query -- TODO replace with appropriate comprarison

--querytest :: NewsQueryParams -> Q Postgres NewsDb s (NewsT (QExpr Postgres s), UserT (QExpr Postgres s), CatT (QExpr Postgres s))
--query :: NewsQueryParams -> Q Postgres NewsDb s (NewsT (QExpr Postgres s), UserT (QExpr Postgres s), CatT (QExpr Postgres s))
queryTest :: NewsQueryParams -> Q Postgres NewsDb s (NewsT (QExpr Postgres s), UserT (QExpr Postgres s), CatT (QExpr Postgres s), QExpr Postgres s Int32, ImageT (QExpr Postgres s))
queryTest params@NewsQueryParams {..} = do
  (news, image) <- filter_ (queryToWhere params) newsImages
  author <- related_ (db ^. nUsers) (_nCreator news)
  cat <- related_ (db ^. nCats) (_nCat news)
  (_, imgCnt) <- filter_ (\(itn, _) -> itn ==. news ^. newsId) (aggregate_ (\itn -> (group_ (itn ^. itnNewsId), as_ @Int32 (count_ (itn ^. itnImageId)))) (all_ (db ^. nImagesToNews)))

  whenJust text $ \(toTextEntry -> text) ->
    guard_
      ( (news ^. newsText `like_` text)
          ||. (author ^. userName `like_` text)
          ||. (cat ^. catName `like_` text)
      )

  whenJust authorName $ \usernameToFind ->
    guard_ (author ^. userName `like_` val_ usernameToFind)

  pure (news, author, cat, imgCnt, image)
