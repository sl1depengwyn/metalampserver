{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Server.Users where

import Crypto.KDF.BCrypt
import Data.Aeson.Extended (FromJSON, ToJSON)
import qualified Data.Aeson.Extended as A
import qualified Data.Pool as Pool
import Data.Time.Calendar
import qualified Database as Db
import Database.Beam
import Database.Beam.Backend
import Database.Beam.Migrate
import Database.Beam.Migrate.Simple
import Database.Beam.Postgres
import qualified Database.Beam.Postgres.Migrate as PG
import Database.Beam.Query.CTE (QAnyScope)
import Database.Beam.Query.Internal
import Database.Migration
import qualified Database.PostgreSQL.Simple as PGS
import Lens.Micro
import qualified Logger
import Servant
import Server
import Universum hiding (Handle, sortBy)

data UserToReturn = UserToReturn
  { utrName :: Text,
    utrDateOfRegistration :: Day
  }
  deriving (Show, Generic)

instance FromJSON UserToReturn where
  parseJSON = A.genericParseJSON (A.customOptionsWithDrop 3)

instance ToJSON UserToReturn where
  toJSON = A.genericToJSON (A.customOptionsWithDrop 3)

userToReturn :: User -> UserToReturn
userToReturn u = UserToReturn {utrName = u ^. userName, utrDateOfRegistration = u ^. userRegDate}

type UsersApi =
  "users" :> Get '[JSON] [UserToReturn]
    :<|> "users" :> "new" :> ReqBody '[JSON] Db.NewUser :> Post '[JSON] Bool

handleGet :: AppM [UserToReturn]
handleGet = pure []

handleNew :: Db.NewUser -> AppM Bool
handleNew newUser = do
  cost <- askCost
  passHash <- liftIO $ hashPassword cost (encodeUtf8 @Text @ByteString (newUser & Db.nuPassword))
  dbh <- askDbh
  liftIO $ Db.addUser dbh (newUser {Db.nuPassword = decodeUtf8 @Text @ByteString passHash})
  pure True

handleUsers :: ServerT UsersApi AppM
handleUsers = handleGet :<|> handleNew