module Adapter.HTTP.API.Routes where



import Web.Scotty.Trans
import ClassyPrelude
import qualified Domain.Server as D
import Adapter.HTTP.API.Common
import Adapter.HTTP.Common
import Text.Printf (printf)
import qualified Data.ByteString.Lazy.Char8 as BS
import System.Random
import Domain.Server (LobbyEntry(..))
import Domain.Game (GameType(..))
import qualified Data.Text.Encoding as T
import Utils.Utils (logWarning, safeRead)
import Data.Aeson (decode, object, (.=), Value, Object, encode)
import qualified Data.Map as Map


mkJsonIntPairStringInt :: (String, Int) -> String
mkJsonIntPairStringInt (name, val) = "\"" ++ name ++ "\":\"" ++ show val ++ "\""

mkJsonIntPairStringString :: (String, String) -> String
mkJsonIntPairStringString (name, val) = "\"" ++ name ++ "\":\"" ++ val ++ "\""

wrapJsonStrings :: [String] -> String
wrapJsonStrings strs = "{" ++ intercalate "," strs ++ "}"

routes :: (MonadUnliftIO m, D.SessionRepo m, D.GameRepo m) => ScottyT m ()
routes = do

  get "/api/getsession" $ do
    (D.SessionId sId, mayUid) <- reqCurrentUserId
    setByteStringValueInCookie "sId" (T.encodeUtf8 $ tshow sId) -- TODO uId and sId saving into sessionStorage via javascript session
    case mayUid of
      Just (D.UserId uId) -> do
        setByteStringValueInCookie "uId" (T.encodeUtf8 $ tshow uId) -- TODO uId and sId saving into sessionStorage via javascript session
        let obj = object ["uId" .= uId, "sId" .= sId] :: Value
        json obj
      Nothing -> do
        let obj = object ["sId" .= sId] :: Value
        json obj
        

  get "/api/lobbytable" $ do
    lobbys <- lift D.getLobbyEntries
    let mkLobbyObj LobbyEntry{lobbyLobbyId, lobbySessionIdHost, lobbyGameType = GameType {gameTypeRules, gameTypeRated}} = 
          object [ "playerName" .= show lobbySessionIdHost
                 , "rating" .= (100 :: Int)
                 , "gameType" .= gameTypeRules
                 , "gameMode" .= (if gameTypeRated then "Rated" else "Casual" :: String)
                 , "link" .= ("/gameroom/" ++ show (D.unLobbyId lobbyLobbyId) :: String)]
    json (map mkLobbyObj lobbys)

  
  get "/api/checklobbygamestatus/:sid" $ do
    sId :: Text <- captureParam "sid"
    liftIO $ putStrLn $ "checklobbygamestatus " ++ sId
    mayGameRoomId <- lift $ D.checkSessionGameStatus (D.SessionId sId)
    liftIO $ putStrLn $ "mayGameRoomId = " ++ tshow mayGameRoomId
    case mayGameRoomId of
      Nothing -> pure ()
      Just (D.GameRoomId roomId) -> json $ "/gameroom/" <> show roomId -- sending text, not JSON


  post "/api/addgametolobby" $ do
    (sId, _) <- reqCurrentUserId
    maySD <- lift $ D.resolveSessionData sId
    case maySD of
      Nothing -> do
        logWarning $ "Adapter.HTTP.API.Routes routes /api/addgametolobby: " <> "sessionData is Nothing, need to relogin"
        pure () -- TODO need to relogin
      Just sd -> do
        eitherLobbyId <- lift $ D.addGameToLobby sId (D._sdWSConn sd) (GameType {gameTypeRules = 10, gameTypeRated = True}) -- TODO set correct rules and rated
        case eitherLobbyId of
          Left lobbyErr -> do
            logWarning $ "Adapter.HTTP.API.Routes routes /api/addgametolobby: " <> tshow lobbyErr
            pure ()
          Right (D.LobbyId lobbyId) -> do
            let obj = object ["lobbyId" .= lobbyId] :: Value
            json obj

  post "/api/joingame" $ do
    -- joinGameReq :: Text <- jsonData
    joinGameReq <- body
    let mayLobbyId = safeRead $ (unpack . decodeUtf8) joinGameReq :: Maybe Int
    case mayLobbyId of
      Nothing -> pure ()
      Just lbId -> do
        let lobbyId = D.LobbyId lbId
        lobbyIsActive <- lift $ D.checkGameInLobby lobbyId
        if not lobbyIsActive
          then pure () -- then redirect "/lobby"
          else do
            maySidUid <- getCurrentUserId
            case maySidUid of
              Nothing -> pure () -- redirect "/auth"
              Just (sId,_) -> do
                maySD <- lift $ D.resolveSessionData sId
                case maySD of
                  Nothing -> pure () -- redirect "/auth"
                  Just sd
                    | isJust (D._sdMayActiveGame sd) -> pure () -- redirect to lobby
                    | otherwise -> do
                      _ <- lift $ D.joinGame sId (D._sdWSConn sd) lobbyId
                      pure ()

  get "/api/sessionrepo" $ do
    ss <- lift D.debugGetAllSessions
    let obj = object ["sessionIds" .= map D.unSessionId (Map.keys ss)] :: Value
    json obj
