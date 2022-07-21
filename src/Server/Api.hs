{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Server.Api where

import Crypto.KDF.BCrypt
import qualified Database as Db
import Database.Migration
import GHC.Base (Symbol)
import Network.Wai.Handler.Warp
import Servant
import Server
import Server.News
import Universum hiding (Handle)

type Api = BasicAuth "NewsAPI" (Maybe User) :> NewsApi

api :: Proxy Api
api = Proxy

server :: ServerT Api AppM
server user = handleNews

app :: Handle -> Application
app h = serveWithContext api ctx (hoistServerWithContext api ctx' (`runReaderT` h) server)
  where
    ctx' :: Proxy '[BasicAuthCheck (Maybe User)]
    ctx' = Proxy

    ctx = checkBasicAuth h :. EmptyContext

startApp :: Handle -> IO ()
startApp h = run port (app h)
  where
    port = cPort (hConfig h)

checkBasicAuth :: Handle -> BasicAuthCheck (Maybe User)
checkBasicAuth h = BasicAuthCheck $ \basicAuthData -> do
  let dbh = hDatabase h
      login = decodeUtf8 (basicAuthUsername basicAuthData)
      password = basicAuthPassword basicAuthData
  mbUser <- Db.getUser dbh login
  case mbUser of
    Nothing -> pure (Authorized Nothing)
    (Just user) ->
      if validatePassword password (encodeUtf8 @Text @ByteString (user ^. userPassword))
        then pure (Authorized (Just user))
        else pure (Authorized Nothing)