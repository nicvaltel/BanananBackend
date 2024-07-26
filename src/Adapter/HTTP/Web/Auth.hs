module Adapter.HTTP.Web.Auth where

import ClassyPrelude
import Web.Scotty.Trans
import Domain.Server
import Network.HTTP.Types.Status
import Network.Wai
import Adapter.HTTP.Web.Common
import Text.Blaze.Html5 ((!))
-- import qualified Text.Digestive.Blaze.Html5 as DH
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A



routes :: (MonadUnliftIO m) => ScottyT m ()
routes = do
  -- home
  get "/" $
    file "static/index.html"
    -- redirect "/api/auth/login"

  get "/api/auth/login" $
    redirect "/api/auth/login"