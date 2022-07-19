{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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
import qualified Database.Beam.Postgres.Migrate as PG
import Database.Migration
import qualified Database.PostgreSQL.Simple as PGS
import Lens.Micro
import qualified Logger
import Universum hiding (Handle)

data Sorting = Date | Author | Cat | Images

data NewsQueryParams = NewsQueryParams
  { createdAt :: Maybe Day,
    createdUntil :: Maybe Day,
    createdSince :: Maybe Day,
    authorName :: Maybe Text,
    cat :: Maybe Int,
    title :: Maybe Text,
    text :: Maybe Text,
    find :: Maybe Text,
    sortBy :: Maybe Sorting
  }

data PostToReturn = PostToReturn
  { ptrId :: Int32,
    ptrTitle :: Text,
    ptrDateOfCreation :: Day,
    ptrCreator :: User,
    ptrCat :: CatToReturn,
    ptrText :: Text,
    ptrIsPublished :: Bool
  }
  deriving (Show, Generic)

instance ToJSON PostToReturn where
  toJSON = A.genericToJSON (A.customOptionsWithDrop 3)

data CatToReturn = CatToReturn
  { ctrId :: Int32,
    ctrParent :: CatToReturn
  }
  deriving (Show, Generic)

instance ToJSON CatToReturn where
  toJSON = A.genericToJSON (A.customOptionsWithDrop 3)

postToReturn :: News -> User -> CatToReturn -> PostToReturn
postToReturn news user cat =
  PostToReturn
    { ptrId = unSerial (news ^. newsId),
      ptrTitle = news ^. newsTitle,
      ptrDateOfCreation = news ^. newsCreatedAt,
      ptrCreator = user,
      ptrCat = cat,
      ptrText = news ^. newsText,
      ptrIsPublished = news ^. isNewsPublished
    }

queryToWhere :: NewsQueryParams -> NewsT (QExpr Postgres QBaseScope) -> QExpr Postgres QBaseScope Bool
queryToWhere NewsQueryParams {..} news =
  case mconcat [dateAt, dateUntil, dateSince, catId, titleText, textEntry] of
    x : xs -> foldr1 (&&.) (x :| xs)
    _ -> val_ True
  where
    dateAt = maybe [] (\date -> [val_ date ==. news ^. newsCreatedAt]) createdAt
    dateUntil = maybe [] (\date -> [val_ date <. news ^. newsCreatedAt]) createdUntil
    dateSince = maybe [] (\date -> [val_ date >. news ^. newsCreatedAt]) createdSince
    catId = maybe [] (\(fromIntegral -> ci) -> [val_ ci ==. news ^. newsCat]) cat
    titleText = maybe [] (\t -> [news ^. newsTitle `like_` val_ ("%" <> t <> "%")]) title
    textEntry = maybe [] (\t -> [news ^. newsText `like_` val_ ("%" <> t <> "%")]) text

-- findAnywhere = maybe [] (\t -> [val_ t ==. news ^. newsText]) find
--authorName = maybe (val_ True) (\a -> news ^. newsTitle `like_` ("%" <> a <> "%")) author

getNews' :: MonadBeam Postgres m => NewsQueryParams -> m [News]
getNews' params@NewsQueryParams {..} = runSelectReturningList $
  select $ do
    news <- filter_ (queryToWhere params) (all_ (db ^. nNews))
    author <- related_ (db ^. nUsers) (_nCreator news)
    cat <- related_ (db ^. nCats) (_nCat news)

    whenJust text $ \(toTextEntry -> text) ->
      guard_
        ( (news ^. newsText `like_` text)
            ||. (author ^. userName `like_` text)
            ||. (cat ^. catName `like_` text)
        )

    whenJust authorName $ \(toTextEntry -> usernameToFind) ->
      guard_ (author ^. userName `like_` usernameToFind)

    pure news
  where
    toTextEntry txt = val_ ("%" <> txt <> "%")
