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
import Utils.Utils (logWarning)
import Data.Aeson (decode, object, (.=), Value)


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
    setByteStringValueInCookie "sId" (T.encodeUtf8 $ tshow sId)
    case mayUid of
      Just (D.UserId uId) -> do
        setByteStringValueInCookie "uId" (T.encodeUtf8 $ tshow uId) -- new token generated
        let obj = object ["uId" .= uId, "sId" .= sId] :: Value
        json obj
      Nothing -> do
        let obj = object ["sId" .= sId] :: Value
        json obj
        pure ()
        

  get "/api/lobbytable" $ do
    lobbys <- lift D.getLobbyEntries
    let lobbyJsonByteStr = lobbysToJsonString lobbys
    let newsJsonObject = case decode lobbyJsonByteStr of
                           Just obj -> obj
                           Nothing -> error "Failed to decode JSON mockLobbyJsonByteStr"
    json (newsJsonObject :: Value)
  
  get "/api/checklobbygamestatus/:lobbyid" $ do
    lobbyId :: Int <- captureParam "lobbyid"
    liftIO $ putStrLn $ "checklobbygamestatus " ++ tshow lobbyId
    mayGameRoomId <- lift $ D.checkLobbyGameStatus (D.LobbyId lobbyId)
    liftIO $ putStrLn $ "mayGameRoomId = " ++ tshow mayGameRoomId
    case mayGameRoomId of
      Nothing -> pure ()
      Just (D.GameRoomId roomId) -> json $ "/gameroom/" <> show roomId
        -- do
        -- let strJson = wrapJsonStrings $ map mkJsonIntPairStringString [("url",  "/gameroom/" <> show roomId)]
        -- json strJson 


  post "/api/addgametolobby" $ do
    undefined
    -- (sId, _, _) <- reqCurrentUserId
    -- eitherLobbyId <- lift $ D.addGameToLobby sId (GameType {gameTypeRules = 10, gameTypeRated = True})
    -- case eitherLobbyId of
    --   Left lobbyErr -> print lobbyErr
    --   -- Right (D.LobbyId lobbyId) -> json $ show lobbyId
    --   Right (D.LobbyId lobbyId) -> do
    --     let strJson = wrapJsonStrings $ map mkJsonIntPairStringInt [("lobbyId", lobbyId)]
    --     json strJson 


  post "/api/joingame/:gameUrl" $ do
    gameUrl :: String <- captureParam "gameUrl"
    putStrLn "/api/joingame/:gameUrl"
    putStrLn $ "gameUrl = " <> tshow  gameUrl



lobbysToJsonString :: [D.LobbyEntry] -> BS.ByteString
lobbysToJsonString lobbys = BS.pack $ "[" ++ intercalate "," (map lobbyToStr lobbys) ++ "]"
  where
    lobbyToStr LobbyEntry{lobbyLobbyId, lobbySessionIdHost, lobbyGameType = GameType {gameTypeRules, gameTypeRated}}=
      printf "{\"playerName\": \"%s\", \"rating\": \"%d\", \"gameType\": \"%d\", \"gameMode\": \"%s\", \"link\": \"%s\"}" 
                ("Player_" ++ show lobbySessionIdHost) 
                (100 :: Int) 
                gameTypeRules 
                (if gameTypeRated then "Rated" else "Casual" :: String)
                ("/gameroom/" ++ show (D.unLobbyId lobbyLobbyId) :: String)

mkRandomLobbyTableMock :: IO String
mkRandomLobbyTableMock = do
  player :: String <- (\(n :: Int) -> if n <= 1000 then "Anonymous" else "Player_" ++ show (n - 1000)) <$> randomRIO (1,1500)
  rating :: Int <- randomRIO (1, 100)
  gameType :: Int <- randomRIO (1,10)
  mode :: String <- (\(n :: Int) -> if n <= 7 then "Casual" else "Rated") <$> randomRIO(1,10)
  let str = printf "{\"playerName\": \"%s\", \"rating\": \"%d\", \"gameType\": \"%d\", \"gameMode\": \"%s\"}" player rating gameType mode
  pure str
