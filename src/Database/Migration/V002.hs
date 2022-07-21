{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Migration.V002
  ( module Database.Migration.V001,
    module Database.Migration.V002,
  )
where

import Control.Monad.Writer.Strict
import Data.Aeson.Extended (FromJSON, ToJSON)
import qualified Data.Aeson.Extended as A
import Data.Time.Calendar
import Database.Beam
import Database.Beam.Backend
import Database.Beam.Backend.SQL.AST (TableName (..))
import Database.Beam.Backend.SQL.Types
import Database.Beam.Migrate.Extended
import Database.Beam.Postgres
import Database.Beam.Postgres.Extensions.UuidOssp
import qualified Database.Beam.Postgres.Migrate as PG
import Database.Migration.V001 hiding
  ( Cat,
    CatId,
    CatT,
    ImageToNews,
    ImageToNewsId,
    ImageToNewsT,
    News,
    NewsDb,
    NewsId,
    NewsT,
    catId,
    catName,
    catParent,
    isNewsPublished,
    itnImageId,
    itnNewsId,
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
    _itnImageId,
    _itnNewsId,
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
  primaryKey = CatId . _cId

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
  (CatId (LensFor catParent)) = tableLenses

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
  primaryKey = NewsId . _nId

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
  (CatId (LensFor newsCat))
  (LensFor newsText)
  (LensFor isNewsPublished) = tableLenses

-- | Image to News type
data ImageToNewsT f = ImageToNews
  { _itnImageId :: PrimaryKey ImageT f,
    _itnNewsId :: PrimaryKey NewsT f
  }
  deriving (Generic, Beamable)

type ImageToNews = ImageToNewsT Identity

deriving instance Show ImageToNews

deriving instance Eq ImageToNews

instance FromJSON ImageToNews where
  parseJSON = A.genericParseJSON (A.customOptionsWithDrop 4)

instance ToJSON ImageToNews where
  toJSON = A.genericToJSON (A.customOptionsWithDrop 4)

instance Table ImageToNewsT where
  data PrimaryKey ImageToNewsT f = ImageToNewsId (PrimaryKey ImageT f) (PrimaryKey NewsT f)
    deriving (Generic, Beamable)
  primaryKey = ImageToNewsId <$> _itnImageId <*> _itnNewsId

deriving instance Show (PrimaryKey ImageToNewsT Identity)

deriving instance Eq (PrimaryKey ImageToNewsT Identity)

instance FromJSON (PrimaryKey ImageToNewsT Identity) where
  parseJSON = A.genericParseJSON (A.customOptionsWithDrop 4)

instance ToJSON (PrimaryKey ImageToNewsT Identity) where
  toJSON = A.genericToJSON (A.customOptionsWithDrop 4)

ImageToNews
  (ImageId (LensFor itnImageId))
  (NewsId (LensFor itnNewsId)) = tableLenses

data NewsDb f = NewsDb
  { _nUsers :: f (TableEntity V001.UserT),
    _nCats :: f (TableEntity CatT),
    _nNews :: f (TableEntity NewsT),
    _nImages :: f (TableEntity V001.ImageT),
    _nImagesToNews :: f (TableEntity ImageToNewsT)
  }
  deriving (Generic, Database Postgres)

NewsDb
  (TableLens nUsers)
  (TableLens nCats)
  (TableLens nNews)
  (TableLens nImages)
  (TableLens nImagesToNews) = dbLenses

retable ::
  Table tbl =>
  CheckedDatabaseEntity Postgres db (TableEntity tbl) ->
  Migration Postgres (CheckedDatabaseEntity Postgres db' (TableEntity tbl))
retable t = alterTable t pure

migration ::
  CheckedDatabaseSettings Postgres V001.NewsDb ->
  Migration Postgres (CheckedDatabaseSettings Postgres NewsDb)
migration oldDb =
  do
    catsWithNoNotNull <- alterTable (oldDb ^. V001.nCats) $
      \oldCats -> do
        asd <- renameColumnTo "" (oldCats ^. V001.catParent)
        nullableParent <- dropNotNullColumn (oldCats ^. V001.catParent)
        pure $
          Cat
            { _cId = oldCats ^. V001.catId,
              _cName = oldCats ^. V001.catName,
              _cParent = CatId nullableParent
            }
    newNews <- alterTable (oldDb ^. V001.nNews) $
      \oldNews@V001.News {..} -> do
        pure $
          News {_nCat = CatId (oldNews ^. V001.newsCat), ..}
    newImagesToNews <- alterTable (oldDb ^. V001.nImagesToNews) $ \olditn ->
      pure $
        ImageToNews
          { _itnImageId = ImageId (olditn ^. V001.itnImageId),
            _itnNewsId = NewsId (olditn ^. V001.itnNewsId)
          }
    users <- preserve (oldDb ^. V001.nUsers)
    news <- preserve (oldDb ^. V001.nNews)
    images <- preserve (oldDb ^. V001.nImages)
    pure $
      NewsDb
        { _nUsers = users,
          _nCats = catsWithNoNotNull,
          _nNews = newNews,
          _nImages = images,
          _nImagesToNews = newImagesToNews
        }
