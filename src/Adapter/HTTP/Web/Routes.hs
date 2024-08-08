module Adapter.HTTP.Web.Routes where

import ClassyPrelude
import Web.Scotty.Trans
import Network.HTTP.Types.Status
import Network.Wai
import Adapter.HTTP.Web.Common
import Text.Blaze.Html5 ((!))
-- import qualified Text.Digestive.Blaze.Html5 as DH
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Domain.Server as D
-- import qualified Web.Scotty.Trans as Sc


routes :: (MonadUnliftIO m, D.GameRepo m) => ScottyT m ()
routes = do
  -- home
  get "/" $
    -- trace "HERE1: Adapter.HTTP.Web.Auth.routes:get '/'"
    redirect "/auth"

  get "/lobby" $
    file "static/lobby.html"

  get "/auth" $
    file "static/auth.html"

  get "/gameroom/:lobbyid" $ do
    lobbyId :: Int <- captureParam "lobbyid"
    lobbyIsActive <- lift $ D.checkGameInLobby (D.LobbyId lobbyId)
    if lobbyIsActive
      then
        file "static/game.html"
      else
        redirect "/lobby"
