{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Migration.V002
  ( module Database.Migration.V001,
    module Database.Migration.V002,
  )
where

import Data.Aeson.Extended (FromJSON, ToJSON)
import qualified Data.Aeson.Extended as A
import Data.Time.Calendar
import Database.Beam
import Database.Beam.Backend
import Database.Beam.Backend.SQL.Types
import Database.Beam.Migrate
import Database.Beam.Postgres
import Database.Beam.Postgres.Extensions.UuidOssp
import Database.Migration.V001 hiding
  ( Cat,
    CatId,
    CatT,
    News,
    NewsDb,
    NewsId,
    NewsT,
    catId,
    catName,
    catParent,
    isNewsPublished,
    migration,
    nCats,
    nImages,
    nImagesToNews,
    nNews,
    nUsers,
    newsCat,
    newsCreatedAt,
    newsCreator,
    newsId,
    newsText,
    newsTitle,
    _cId,
    _cName,
    _cParent,
    _mIsPublished,
    _nCat,
    _nCats,
    _nCreator,
    _nDateOfCreation,
    _nId,
    _nImages,
    _nImagesToNews,
    _nNews,
    _nText,
    _nTitle,
    _nUsers,
  )
import qualified Database.Migration.V001 as V001
import Universum

-- | Category type
data CatT f = Cat
  { _cId :: Columnar f (SqlSerial Int32),
    _cName :: Columnar f Text,
    _cParent :: PrimaryKey CatT (Nullable f)
  }
  deriving (Generic, Beamable)

type Cat = CatT Identity

deriving instance Show Cat

deriving instance Eq Cat

instance FromJSON Cat where
  parseJSON = A.genericParseJSON A.customOptions

instance ToJSON Cat where
  toJSON = A.genericToJSON A.customOptions

instance Table CatT where
  data PrimaryKey CatT f = CatId (Columnar f (SqlSerial Int32))
    deriving (Generic, Beamable)
  primaryKey = Database.Migration.V002.CatId . Database.Migration.V002._cId

deriving instance Show (PrimaryKey CatT (Nullable Identity))

deriving instance Eq (PrimaryKey CatT (Nullable Identity))

deriving instance Show (PrimaryKey CatT Identity)

deriving instance Eq (PrimaryKey CatT Identity)

instance FromJSON (PrimaryKey CatT (Nullable Identity)) where
  parseJSON = A.genericParseJSON A.customOptions

instance ToJSON (PrimaryKey CatT (Nullable Identity)) where
  toJSON = A.genericToJSON A.customOptions

instance FromJSON (PrimaryKey CatT Identity) where
  parseJSON = A.genericParseJSON A.customOptions

instance ToJSON (PrimaryKey CatT Identity) where
  toJSON = A.genericToJSON A.customOptions

Cat
  (LensFor catId)
  (LensFor catName)
  (Database.Migration.V002.CatId (LensFor catParent)) = tableLenses

-- | News type
data NewsT f = News
  { _nId :: Columnar f (SqlSerial Int32),
    _nTitle :: Columnar f Text,
    _nDateOfCreation :: Columnar f Day,
    _nCreator :: PrimaryKey V001.UserT f,
    _nCat :: PrimaryKey CatT f,
    _nText :: Columnar f Text,
    _mIsPublished :: Columnar f Bool
  }
  deriving (Generic, Beamable)

type News = NewsT Identity

deriving instance Show News

deriving instance Eq News

instance FromJSON News where
  parseJSON = A.genericParseJSON A.customOptions

instance ToJSON News where
  toJSON = A.genericToJSON A.customOptions

instance Table NewsT where
  data PrimaryKey NewsT f = NewsId (Columnar f (SqlSerial Int32))
    deriving (Generic, Beamable)
  primaryKey = Database.Migration.V002.NewsId . Database.Migration.V002._nId

deriving instance Show (PrimaryKey NewsT Identity)

deriving instance Eq (PrimaryKey NewsT Identity)

instance FromJSON (PrimaryKey NewsT Identity) where
  parseJSON = A.genericParseJSON A.customOptions

instance ToJSON (PrimaryKey NewsT Identity) where
  toJSON = A.genericToJSON A.customOptions

News
  (LensFor newsId)
  (LensFor newsTitle)
  (LensFor newsCreatedAt)
  (V001.UserId (LensFor newsCreator))
  (Database.Migration.V002.CatId (LensFor newsCat))
  (LensFor newsText)
  (LensFor isNewsPublished) = tableLenses

data NewsDb f = NewsDb
  { _nUsers :: f (TableEntity V001.UserT),
    _nCats :: f (TableEntity CatT),
    _nNews :: f (TableEntity NewsT),
    _nImages :: f (TableEntity V001.ImageT),
    _nImagesToNews :: f (TableEntity V001.ImageToNewsT)
  }
  deriving (Generic, Database Postgres)

NewsDb
  (TableLens nUsers)
  (TableLens nCats)
  (TableLens nNews)
  (TableLens nImages)
  (TableLens nImagesToNews) = dbLenses

migration ::
  CheckedDatabaseSettings Postgres V001.NewsDb ->
  Migration Postgres (CheckedDatabaseSettings Postgres NewsDb)
migration oldDb =
  NewsDb
    <$> preserve (oldDb ^. V001.nUsers)
    <*> ( createTable "categories" $
            Cat
              { Database.Migration.V002._cId = field "id" serial notNull unique,
                Database.Migration.V002._cName = field "name" (V001.varcharOf 64) notNull,
                Database.Migration.V002._cParent = Database.Migration.V002.CatId (field "parent_id" (maybeType serial))
              }
        )
    <*> ( createTable "news" $
            News
              { Database.Migration.V002._nId = field "id" serial notNull unique,
                Database.Migration.V002._nTitle = field "title" (V001.varcharOf 128) notNull,
                Database.Migration.V002._nDateOfCreation = field "creation_date" date notNull,
                Database.Migration.V002._nCreator = V001.UserId (field "creator_id" serial notNull),
                Database.Migration.V002._nCat = Database.Migration.V002.CatId (field "category_id" serial notNull),
                Database.Migration.V002._nText = field "text" (varchar Nothing) notNull,
                Database.Migration.V002._mIsPublished = field "is_published" boolean notNull
              }
        )
    <*> preserve (oldDb ^. V001.nImages)
    <*> preserve (oldDb ^. V001.nImagesToNews)
