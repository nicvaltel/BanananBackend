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
-- import qualified Web.Scotty.Trans as Sc


routes :: (MonadUnliftIO m, D.SessionRepo m, D.GameRepo m) => ScottyT m ()
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
    undefined
    -- lbId :: Int <- captureParam "lobbyid"
    -- let lobbyId = D.LobbyId lbId
    -- liftIO $ do
    --   print "/gameroom lobbyId = "
    --   print lobbyId
    -- lobbyIsActive <- lift $ D.checkGameInLobby lobbyId
    -- if lobbyIsActive
    --   then do
    --     maySessionIdUserId <- getCurrentUserId
    --     case maySessionIdUserId of
    --       Nothing -> redirect "/auth"
    --       Just (sId, uId) -> do
    --         _ <- lift $ D.joinGame sId lobbyId
    --         file "static/game.html"
    --   else
    --     redirect "/lobby"
