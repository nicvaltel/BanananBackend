{-# LANGUAGE ScopedTypeVariables #-}
module Adapter.HTTP.API.Routes where



import Web.Scotty.Trans
import ClassyPrelude
import qualified Domain.Server as D
import Adapter.HTTP.API.Common
import Adapter.HTTP.Common
import Text.Printf (printf)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Aeson (decode, Value, FromJSON (..), fromJSON)
import System.Random
import Domain.Server (LobbyEntry(..))
import Domain.Game (GameType(..))
import Data.Maybe (fromJust)


-- routes :: (MonadUnliftIO m) => ScottyT m ()
-- routes = do
--   -- register
--   post "/api/auth/register" $ do
--    pure ()

--   -- verify email
--   post "/api/auth/verifyEmail" $ do
--    pure ()

--   -- login 
--   post "/api/auth/login" $ do
--         pure ()

--   -- get user
--   get "/api/users" $ do
--     pure ()




mkJsonIntPairString :: (String, Int) -> String
mkJsonIntPairString (name, val) = "\"" ++ name ++ "\":" ++ show val

wrapJsonStrings :: [String] -> String
wrapJsonStrings strs = "{" ++ intercalate "," strs ++ "}"

routes :: (MonadUnliftIO m, D.SessionRepo m, D.GameRepo m) => ScottyT m ()
routes = do

  get "/api/users" $ do
    (D.SessionId sId, D.UserId uId) <- reqCurrentUserId
    setSessionIdInCookie (D.SessionId sId)
    let strJson = wrapJsonStrings $ map mkJsonIntPairString [("uId", uId), ("sId", sId)]
    json strJson

  get "/api/lobbytable" $ do
    lobbys <- lift D.getLobbyEntries
    let lobbyJsonByteStr = lobbysToJsonString lobbys
    let newsJsonObject = case decode lobbyJsonByteStr of
                           Just obj -> obj
                           Nothing -> error "Failed to decode JSON mockLobbyJsonByteStr"
    json (newsJsonObject :: Value)
  
  post "/api/addgametolobby" $ do
    (sId, uId) <- reqCurrentUserId
    success <- lift $ D.addGameToLobby sId (GameType {gameTypeRules = 10, gameTypeRated = True})
    case success of
      Left lobbyErr -> print lobbyErr
      Right (D.LobbyId lid) -> json $ tshow lid 


lobbysToJsonString :: [D.LobbyEntry] -> BS.ByteString
lobbysToJsonString lobbys = BS.pack $ "[" ++ intercalate "," (map lobbyToStr lobbys) ++ "]"
  where
    lobbyToStr LobbyEntry{lobbySessionIdHost, lobbyGameType = GameType {gameTypeRules, gameTypeRated}}=
      printf "{\"playerName\": \"%s\", \"rating\": \"%d\", \"gameType\": \"%d\", \"gameMode\": \"%s\", \"link\": \"%s\"}" 
                ("Player_" ++ show lobbySessionIdHost) 
                (100 :: Int) 
                gameTypeRules 
                (if gameTypeRated then "Rated" else "Casual" :: String)
                ("/gameroom_" ++ show (D.unSessionId lobbySessionIdHost) :: String)

mkRandomLobbyTableMock :: IO String
mkRandomLobbyTableMock = do
  player :: String <- (\(n :: Int) -> if n <= 1000 then "Anonymous" else "Player_" ++ show (n - 1000)) <$> randomRIO (1,1500)
  rating :: Int <- randomRIO (1, 100)
  gameType :: Int <- randomRIO (1,10)
  mode :: String <- (\(n :: Int) -> if n <= 7 then "Casual" else "Rated") <$> randomRIO(1,10)
  let str = printf "{\"playerName\": \"%s\", \"rating\": \"%d\", \"gameType\": \"%d\", \"gameMode\": \"%s\"}" player rating gameType mode
  pure str
