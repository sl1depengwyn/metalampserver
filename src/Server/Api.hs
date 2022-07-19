module Server.Api where

import qualified Database as DB
import Network.Wai.Handler.Warp
import Servant
import Server
import Server.News
import Universum hiding (Handle)

type Api = NewsApi

api :: Proxy NewsApi
api = Proxy

server :: ServerT NewsApi AppM
server = handleNews

app :: Handle -> Application
app h = serve api (hoistServer api (`runReaderT` h) server)

startApp :: Handle -> IO ()
startApp h = run port (app h)
  where
    port = cPort (hConfig h)