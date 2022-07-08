{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Migration.V001 where

import Data.Aeson.Extended (FromJSON, ToJSON)
import qualified Data.Aeson.Extended as A
import Data.Time
import Database.Beam
import Database.Beam.Backend
import Database.Beam.Migrate
import Database.Beam.Postgres
import Universum

-- | User type
data UserT f = User
  { _uId :: Columnar f Int,
    _uLogin :: Columnar f Text,
    _uPassword :: Columnar f Text,
    _uDateOfRegistration :: Columnar f UTCTime,
    _isAdmin :: Columnar f Bool,
    _canCreate :: Columnar f Bool
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
  data PrimaryKey UserT f = UserId (Columnar f Int)
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
  (LensFor userLogin)
  (LensFor userPassword)
  (LensFor userRegDate)
  (LensFor isUserAdmin)
  (LensFor canUserCreate) = tableLenses

-- | Category type
data CatT f = Cat
  { _cId :: Columnar f Int,
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
  data PrimaryKey CatT f = CatId (Columnar f Int)
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
  (CatId (LensFor catParent)) = tableLenses

-- | News type
data NewsT f = News
  { _nId :: Columnar f Int,
    _nTitle :: Columnar f Text,
    _nDateOfCreation :: Columnar f UTCTime,
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
  data PrimaryKey NewsT f = NewsId (Columnar f Int)
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
  { _iId :: Columnar f Int,
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
  data PrimaryKey ImageT f = ImageId (Columnar f Int)
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

data ImageToNewsT f = ImageToNews
  { _itnId :: Columnar f Int,
    _itnImageId :: PrimaryKey ImageT f,
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
  data PrimaryKey ImageToNewsT f = ImageToNewsId (Columnar f Int)
    deriving (Generic, Beamable)
  primaryKey = ImageToNewsId . _itnId

deriving instance Show (PrimaryKey ImageToNewsT Identity)

deriving instance Eq (PrimaryKey ImageToNewsT Identity)

instance FromJSON (PrimaryKey ImageToNewsT Identity) where
  parseJSON = A.genericParseJSON (A.customOptionsWithDrop 4)

instance ToJSON (PrimaryKey ImageToNewsT Identity) where
  toJSON = A.genericToJSON (A.customOptionsWithDrop 4)

ImageToNews
  (LensFor itnId)
  (ImageId (LensFor itnImageId))
  (NewsId (LensFor itnNewsId)) = tableLenses