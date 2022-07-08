{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Migration.V001 where

import Data.Time
import Database.Beam
import Database.Beam.Backend
import Database.Beam.Migrate
import Database.Beam.Postgres
import Universum

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

instance Table UserT where
  data PrimaryKey UserT f = UserId (Columnar f Int)
    deriving (Generic, Beamable)
  primaryKey = UserId . _uId

deriving instance Show (PrimaryKey UserT Identity)

deriving instance Eq (PrimaryKey UserT Identity)

User
  (LensFor userId)
  (LensFor userLogin)
  (LensFor userPassword)
  (LensFor userRegDate)
  (LensFor isUserAdmin)
  (LensFor canUserCreate) = tableLenses

data CatT f = Cat
  { _cId :: Columnar f Int,
    _cParent :: PrimaryKey CatT f
  }
  deriving (Generic, Beamable)

type Cat = CatT Identity

deriving instance Show Cat

deriving instance Eq Cat

instance Table CatT where
  data PrimaryKey CatT f = CatId (Columnar f Int)
    deriving (Generic, Beamable)
  primaryKey = CatId . _cId

deriving instance Show (PrimaryKey CatT Identity)

deriving instance Eq (PrimaryKey CatT Identity)

Cat
  (LensFor catId)
  (CatId (LensFor catParent)) = tableLenses

data NewsT f = News
  { _nId :: Columnar f Int,
    _nTitle :: Columnar f Text,
    _nDateOfCreation :: Columnar f UTCTime,
    _nCreator :: PrimaryKey UserT f,
    _nCat :: PrimaryKey CatT f,
    _nText :: Columnar f Text,
    _nPictures :: Columnar f [Int],
    _mIsPublished :: Columnar f Bool
  }
  deriving (Generic, Beamable)

type News = NewsT Identity

deriving instance Show News

deriving instance Eq News

instance Table NewsT where
  data PrimaryKey NewsT f = NewsId (Columnar f Int)
    deriving (Generic, Beamable)
  primaryKey = NewsId . _nId

deriving instance Show (PrimaryKey NewsT Identity)

deriving instance Eq (PrimaryKey NewsT Identity)

News
  (LensFor newsId)
  (LensFor newsTitle)
  (LensFor newsCreatedAt)
  (UserId (LensFor newsCreator))
  (CatId (LensFor newsCat))
  (LensFor newsText)
  (LensFor newsPictures)
  (LensFor isNewsPublished) = tableLenses