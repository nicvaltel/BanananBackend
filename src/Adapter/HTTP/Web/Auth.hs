module Adapter.HTTP.Web.Auth where

import Reexport hiding (concatMap, get)
import Web.Scotty.Trans
import Domain.Server
import Web.Scotty.Trans
import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Middleware.Gzip
import Network.Wai.Middleware.Static (staticPolicy', CacheContainer, addBase, initCaching, CachingStrategy (..))
-- import Network.Wai.Handler.Warp ( run )
import Adapter.HTTP.Web.Common
import Text.Blaze.Html5 ((!))
-- import qualified Text.Digestive.Blaze.Html5 as DH
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Data.Aeson (object, (.=))

import ClassyPrelude (concatMap)

routes :: (MonadUnliftIO m) => ScottyT m ()
routes = do
  -- home
  get "/" $
    redirect "/api/auth/login"

  get "/api/auth/login" $
    redirect "/api/auth/login"