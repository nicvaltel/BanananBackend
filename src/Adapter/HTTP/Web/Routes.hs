module Adapter.HTTP.Web.Routes where

import ClassyPrelude
import Web.Scotty.Trans
import Network.HTTP.Types.Status
import Network.Wai
import Adapter.HTTP.Web.Common
import Adapter.HTTP.Common
import Text.Blaze.Html5 ((!))
-- import qualified Text.Digestive.Blaze.Html5 as DH
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Domain.Server as D
import Domain.Server (sdMayActiveGame)
-- import qualified Web.Scotty.Trans as Sc


routes :: (MonadUnliftIO m) => ScottyT m ()
routes = do
  -- home
  get "/" $
    redirect "/auth"

  get "/lobby" $
    file "static/lobby.html"

  get "/auth" $
    file "static/auth.html"

  get "/gameroom/:lobbyid" $ do
    file "static/game.html"
