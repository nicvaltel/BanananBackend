{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Domain.Server 
  ( UserId(..)
  , SessionId(..)
  , SessionIdHost
  , SessionIdGuest
  , GameRoom(..)
  , GameRoomId(..)
  , LobbyId(..)
  , SessionRepo(..)
  , WSRepo(..)
  , GameRepo(..)
  , LobbyError(..)
  , SessionError(..)
  , LobbyEntry(..)
  , generateNewSessionId
  , resolveSessionId
  -- , processOneWSMessageEcho
  , checkGameInLobby
  , checkLobbyGameStatus
  , Token
  -- , initGuestSession
  -- , initRegUserSession
  , lengthOfSessionIdConst
  ) where


import ClassyPrelude
import Domain.Game
import qualified WebSocketServer as WS
import WebSocketServer(WSMessage, WSChan(..), WSConnection)
import qualified Domain.GameBot.GameModel as G
import qualified Data.Text as Text
import Text.StringRandom (stringRandomIO)

type Token = Text
type SessionIdHost = SessionId
type SessionIdGuest = SessionId

newtype SessionId = SessionId {unSessionId :: Text}
  deriving (Show, Eq, Ord)

lengthOfSessionIdConst :: Int
lengthOfSessionIdConst = 32

newtype UserId = UserId {unUserId :: Int}
  deriving (Show, Eq, Ord, Num)

newtype GameRoomId = GameRoomId {unGameRoomId :: Int}
  deriving (Show, Eq, Ord, Num)


newtype LobbyId = LobbyId {unLobbyId :: Int}
  deriving (Show, Eq, Ord, Num)

data GameRoom = GameRoom
  { gameRoomGameType :: GameType
  , gameRoomHost :: SessionId
  , gameRoomGuest :: SessionId
  , gameRoomHostChan :: WSChan  
  , gameRoomGuestChan :: WSChan 
  , gameRoomGameState :: TVar G.GameState
  } 

data LobbyEntry = LobbyEntry 
  { lobbyLobbyId :: LobbyId
  , lobbySessionIdHost :: SessionId
  , lobbyWSConnectionHost :: WSConnection
  , lobbyGameType :: GameType
  , lobbyMaybeGameRoomId :: Maybe GameRoomId
  }

data LobbyError =
    LobbyErrorActiveGameIsGoingOn
  | LobbyErrorGameOrderIsInTheLobby
    deriving (Show, Eq, Ord)

data SessionError =
    SessionErrorSessionIdIsNotActive
  | SessionErrorGameRoomIdIsNotActive
  | SessionErrorWSConnectionIsAlreadyActive
    deriving(Show, Eq, Ord)

class Monad m => SessionRepo m where
  newSession :: Maybe UserId -> SessionId -> WSConnection -> m (Either SessionError (TVar WSChan))
  findUserBySessionId :: SessionId -> m (Maybe UserId)

class Monad m => WSRepo m where
  -- initWSConn :: SessionId -> m (Either SessionError (TVar WSChan))
  disconnectWSConn :: WSConnection -> SessionId -> m ()
  -- pushInputWSMessage :: SessionId -> GameRoomId -> WSMessage -> m ()
  -- processWSMessages :: (SessionId -> GameRoomId -> WSMessage -> m (Maybe (WSConnection, WSMessage))) -> SessionId -> GameRoomId -> m () -- TODO move partially it ot game logic
  -- sendOutWSMessage :: SessionId -> GameRoomId -> m ()
  -- sendOutAllWSMessages :: m ()
  -- getWSChanBySessionId :: SessionId -> m (Maybe WSChan)

class Monad m => GameRepo m where
  addGameToLobby :: SessionId -> WSConnection -> GameType -> m (Either LobbyError LobbyId)
  getLobbyEntries :: m [LobbyEntry]
  joinGame :: SessionIdGuest -> WSConnection -> LobbyId -> m (Maybe GameRoomId)
  -- startGameWithBot :: SessionId -> GameType -> m (Either LobbyError GameRoomId)

generateNewSessionId :: MonadIO m => m SessionId
generateNewSessionId = do
  let randLen = tshow lengthOfSessionIdConst
  sId <- liftIO $ stringRandomIO ("[A-Za-z0-9]{" <> randLen <> "}")
  pure (SessionId sId)

-- initGuestSession :: SessionRepo m => m SessionId
-- initGuestSession = newSession Nothing

-- initRegUserSession :: SessionRepo m => UserId -> m SessionId
-- initRegUserSession = newSession . Just

resolveSessionId :: SessionRepo m => SessionId -> m (Maybe UserId)
resolveSessionId = findUserBySessionId



--------------------------------------------------


-- processOneWSMessageEcho :: (SessionRepo m, WSRepo m) => SessionId -> GameRoomId -> WS.WSMessage -> m (Maybe (WS.WSConnection, WS.WSMessage))
-- processOneWSMessageEcho sId gId wsmsg = do
--   case Text.splitAt 5 wsmsg of
--     ("lobb:", msg) -> pure Nothing
--     ("chat:", msg) -> pure Nothing
--     ("echo:", msg) -> do
--       mayChan <- getWSChanBySessionId sId
--       case mayChan of
--         Just WSChan{wschanConn} -> pure $ Just (wschanConn, msg)
--         Nothing -> pure Nothing
--     _ -> do -- no prefix for active Game ws messages
--       mayChan <- getWSChanBySessionId sId
--       case mayChan of
--         Just WSChan{wschanConn} -> pure $ Just (wschanConn, wsmsg)
--         Nothing -> pure Nothing
        

checkGameInLobby :: GameRepo m => LobbyId -> m Bool
checkGameInLobby lbId = any ((==) lbId . lobbyLobbyId) <$> getLobbyEntries


checkLobbyGameStatus :: GameRepo m => LobbyId -> m (Maybe GameRoomId)
checkLobbyGameStatus lbId = do
  lobbys <- getLobbyEntries
  case filter ((==) lbId . lobbyLobbyId) lobbys of
    [lobby] -> pure (lobbyMaybeGameRoomId lobby)
    _ -> pure Nothing