{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Migration.V001 where

import Data.Aeson.Extended (FromJSON, ToJSON)
import qualified Data.Aeson.Extended as A
import Data.Time.Calendar
import Database.Beam
import Database.Beam.Backend
import Database.Beam.Backend.SQL.Types
import Database.Beam.Migrate
import Database.Beam.Postgres
import Database.Beam.Postgres.Extensions.UuidOssp
import Universum

-- | User type
data UserT f = User
  { _uId :: Columnar f (SqlSerial Int32),
    _uName :: Columnar f Text,
    _uLogin :: Columnar f Text,
    _uPassword :: Columnar f Text,
    _uDateOfRegistration :: Columnar f Day,
    _uIsAdmin :: Columnar f Bool,
    _uCanCreate :: Columnar f Bool
  }
  deriving (Generic, Beamable)

type User = UserT Identity

deriving instance Show User

deriving instance Eq User

instance FromJSON User where
  parseJSON = A.genericParseJSON A.customOptions

instance ToJSON User where
  toJSON = A.genericToJSON A.customOptions

instance Table UserT where
  data PrimaryKey UserT f = UserId (Columnar f (SqlSerial Int32))
    deriving (Generic, Beamable)
  primaryKey = UserId . _uId

deriving instance Show (PrimaryKey UserT Identity)

deriving instance Eq (PrimaryKey UserT Identity)

instance FromJSON (PrimaryKey UserT Identity) where
  parseJSON = A.genericParseJSON A.customOptions

instance ToJSON (PrimaryKey UserT Identity) where
  toJSON = A.genericToJSON A.customOptions

User
  (LensFor userId)
  (LensFor userName)
  (LensFor userLogin)
  (LensFor userPassword)
  (LensFor userRegDate)
  (LensFor isUserAdmin)
  (LensFor canUserCreate) = tableLenses

-- | Category type
data CatT f = Cat
  { _cId :: Columnar f (SqlSerial Int32),
    _cName :: Columnar f Text,
    _cParent :: PrimaryKey CatT f
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

deriving instance Show (PrimaryKey CatT Identity)

deriving instance Eq (PrimaryKey CatT Identity)

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
    _nCreator :: PrimaryKey UserT f,
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
  (UserId (LensFor newsCreator))
  (CatId (LensFor newsCat))
  (LensFor newsText)
  (LensFor isNewsPublished) = tableLenses

-- Image type
data ImageT f = Image
  { _iId :: Columnar f (SqlSerial Int32),
    _iData :: Columnar f Text
  }
  deriving (Generic, Beamable)

type Image = ImageT Identity

deriving instance Show Image

deriving instance Eq Image

instance FromJSON Image where
  parseJSON = A.genericParseJSON A.customOptions

instance ToJSON Image where
  toJSON = A.genericToJSON A.customOptions

instance Table ImageT where
  data PrimaryKey ImageT f = ImageId (Columnar f (SqlSerial Int32))
    deriving (Generic, Beamable)
  primaryKey = ImageId . _iId

deriving instance Show (PrimaryKey ImageT Identity)

deriving instance Eq (PrimaryKey ImageT Identity)

instance FromJSON (PrimaryKey ImageT Identity) where
  parseJSON = A.genericParseJSON A.customOptions

instance ToJSON (PrimaryKey ImageT Identity) where
  toJSON = A.genericToJSON A.customOptions

Image
  (LensFor imageId)
  (LensFor imageData) = tableLenses

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

-- | News DataBase
data NewsDb f = NewsDb
  { _nUsers :: f (TableEntity UserT),
    _nCats :: f (TableEntity CatT),
    _nNews :: f (TableEntity NewsT),
    _nImages :: f (TableEntity ImageT),
    _nImagesToNews :: f (TableEntity ImageToNewsT)
  }
  deriving (Generic, Database Postgres)

NewsDb
  (TableLens nUsers)
  (TableLens nCats)
  (TableLens nNews)
  (TableLens nImages)
  (TableLens nImagesToNews) = dbLenses

migration :: () -> Migration Postgres (CheckedDatabaseSettings Postgres NewsDb)
migration () =
  NewsDb
    <$> ( createTable "users" $
            User
              { _uId = field "id" serial notNull unique,
                _uName = field "name" text notNull,
                _uLogin = field "login" text notNull,
                _uPassword = field "password" text notNull,
                _uDateOfRegistration = field "registration_date" date notNull,
                _uIsAdmin = field "is_admin" boolean notNull,
                _uCanCreate = field "can_create" boolean notNull
              }
        )
    <*> ( createTable "categories" $
            Cat
              { _cId = field "id" serial notNull unique,
                _cName = field "name" text notNull,
                _cParent = CatId (field "parent_id" serial notNull)
              }
        )
    <*> ( createTable "news" $
            News
              { _nId = field "id" serial notNull unique,
                _nTitle = field "title" text notNull,
                _nDateOfCreation = field "creation_date" date notNull,
                _nCreator = UserId (field "creator_id" serial notNull),
                _nCat = CatId (field "category_id" serial notNull),
                _nText = field "text" (varchar Nothing) notNull,
                _mIsPublished = field "is_published" boolean notNull
              }
        )
    <*> ( createTable "images" $
            Image
              { _iId = field "id" serial notNull unique,
                _iData = field "data" (varchar Nothing) notNull
              }
        )
    <*> ( createTable "image_to_news" $
            ImageToNews
              { _itnImageId = ImageId (field "image_id" serial notNull),
                _itnNewsId = NewsId (field "news_id" serial notNull)
              }
        )
