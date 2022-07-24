{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Migration.V003
  ( module Database.Migration.V002,
    module Database.Migration.V003,
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
import Database.Migration.V002 hiding
  ( ImageToNews,
    ImageToNewsId,
    ImageToNewsT,
    News,
    NewsDb,
    NewsId,
    NewsT,
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
import qualified Database.Migration.V002 as V002
import Universum

-- | News type
data NewsT f = News
  { _nId :: Columnar f (SqlSerial Int32),
    _nTitle :: Columnar f Text,
    _nDateOfCreation :: Columnar f Day,
    _nCreator :: PrimaryKey V002.UserT f,
    _nCat :: PrimaryKey CatT f,
    _nText :: Columnar f Text,
    _nNoPictures :: Columnar f Int32,
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
  (V002.UserId (LensFor newsCreator))
  (CatId (LensFor newsCat))
  (LensFor newsText)
  (LensFor newsNoPictures)
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
  { _nUsers :: f (TableEntity V002.UserT),
    _nCats :: f (TableEntity V002.CatT),
    _nNews :: f (TableEntity NewsT),
    _nImages :: f (TableEntity V002.ImageT),
    _nImagesToNews :: f (TableEntity ImageToNewsT)
  }
  deriving (Generic, Database Postgres)

NewsDb
  (TableLens nUsers)
  (TableLens nCats)
  (TableLens nNews)
  (TableLens nImages)
  (TableLens nImagesToNews) = dbLenses

migration ::
  CheckedDatabaseSettings Postgres V002.NewsDb ->
  Migration Postgres (CheckedDatabaseSettings Postgres NewsDb)
migration oldDb =
  do
    newNews <- alterTable (oldDb ^. V002.nNews) $
      \oldNews@V002.News {..} -> do
        _nNoPictures <- addColumn (field "no_pictures" int notNull)
        pure $
          News {_nCat = CatId (oldNews ^. V002.newsCat), ..}
    newImagesToNews <- alterTable (oldDb ^. V002.nImagesToNews) $ \olditn ->
      pure $
        ImageToNews
          { _itnImageId = ImageId (olditn ^. V002.itnImageId),
            _itnNewsId = NewsId (olditn ^. V002.itnNewsId)
          }
    users <- preserve (oldDb ^. V002.nUsers)
    news <- preserve (oldDb ^. V002.nNews)
    images <- preserve (oldDb ^. V002.nImages)
    cats <- preserve (oldDb ^. V002.nCats)
    pure $
      NewsDb
        { _nUsers = users,
          _nCats = cats,
          _nNews = newNews,
          _nImages = images,
          _nImagesToNews = newImagesToNews
        }
