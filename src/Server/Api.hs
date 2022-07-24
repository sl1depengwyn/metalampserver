{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Server.Api where

import Crypto.KDF.BCrypt
import Data.Aeson.Extended (FromJSON, ToJSON)
import qualified Data.Aeson.Extended as A
import qualified Database as Db
import Database.Migration
import Network.Wai.Handler.Warp
import Servant hiding (BasicAuth, BasicAuthResult (..))
import Servant.Auth
import Servant.Auth.Server
import Server
import Server.News
import Server.Users
import Universum hiding (Handle)

data UserCreds = UserCreds
  { ucLogin :: Text,
    ucPassword :: Text
  }
  deriving (Generic, Show)

instance FromJSON UserCreds where
  parseJSON = A.genericParseJSON A.customOptions

instance ToJSON UserCreds where
  toJSON = A.genericToJSON A.customOptions

instance ToJWT User

instance FromJWT User

type instance BasicAuthCfg = BasicAuthData -> IO (AuthResult User)

instance FromBasicAuthData User where
  fromBasicAuthData authData authCheckFunction = authCheckFunction authData

type Api = Auth '[BasicAuth] User :> (NewsApi :<|> UsersApi)

api :: Proxy Api
api = Proxy

server :: ServerT Api AppM
server user = handleNews :<|> handleUsers

mkApp :: Handle -> IO Application
mkApp h = do
  k <- generateKey
  let jwtCfg = defaultJWTSettings k
      authCfg = checkBasicAuth h
      ctx = jwtCfg :. defaultCookieSettings :. authCfg :. EmptyContext
  pure $ serveWithContext api ctx (hoistServerWithContext api ctx' (`runReaderT` h) server)
  where
    ctx' :: Proxy '[JWTSettings, CookieSettings, BasicAuthCfg]
    ctx' = Proxy

startApp :: Handle -> IO ()
startApp h = run port =<< mkApp h
  where
    port = cPort (hConfig h)

checkBasicAuth :: Handle -> BasicAuthData -> IO (AuthResult User)
checkBasicAuth h basicAuthData = do
  let dbh = hDatabase h
      login = decodeUtf8 (basicAuthUsername basicAuthData)
      password = basicAuthPassword basicAuthData
  mbUser <- Db.getUser dbh login
  case mbUser of
    Nothing -> pure NoSuchUser
    (Just user) ->
      if validatePassword password (encodeUtf8 @Text @ByteString (user ^. userPassword))
        then pure (Authenticated user)
        else pure BadPassword