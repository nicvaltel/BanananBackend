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
  , resolveSessionId
  , processOneWSMessageEcho
  ) where


import Reexport
import ClassyPrelude
import Domain.Game
import qualified WebSocketServer as WS
import WebSocketServer(WSMessage, WSChan(..), WSConnection)
import qualified Domain.GameBot.GameModel as G
import qualified Data.Text as Text


type SessionIdHost = SessionId
type SessionIdGuest = SessionId
newtype SessionId = SessionId {unSessionId :: Int}
  deriving (Show, Eq, Ord, Num)

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
  } deriving (Show, Eq, Ord)

data LobbyError =
    LobbyErrorActiveGameIsGoingOn
  | LobbyErrorGameOrderIsInTheLobby
    deriving (Show, Eq, Ord)

data SessionError =
  SessionErrorSessionIdIsNotActive
    deriving(Show, Eq, Ord)

class Monad m => SessionRepo m where
  initNewGuestSession :: m (SessionId, UserId)
  initKnownUserSession :: UserId -> m (Maybe (SessionId, UserId))
  restoreExistingSession :: SessionId -> UserId -> m (Maybe (SessionId, UserId))
  disconnectSession :: SessionId -> m ()
  initBotSession :: m (SessionId, UserId)
  getUserIdBySessionId :: SessionId -> m (Maybe UserId)


class Monad m => WSRepo m where
  initWSConn :: WSConnection -> SessionId -> m (Either SessionError ())
  disconnectWSConn :: WSConnection -> SessionId -> m ()
  pushInputWSMessage :: SessionId -> WSMessage -> m ()
  processWSMessages :: (SessionId -> WSMessage -> m (Maybe (WSConnection, WSMessage))) -> SessionId -> m () -- TODO move partially it ot game logic
  sendOutWSMessage :: SessionId -> m ()
  sendOutAllWSMessages :: m ()
  getWSChanBySessionId :: SessionId -> m (Maybe WSChan)

class Monad m => GameRepo m where
  addGameToLobby :: SessionId -> GameType -> m (Either LobbyError LobbyId)
  joinGame :: SessionIdGuest -> LobbyId -> m (Maybe GameRoomId)
  startGameWithBot :: SessionId -> GameType -> m (Either LobbyError GameRoomId)

resolveSessionId :: SessionRepo m => SessionId -> m (Maybe UserId)
resolveSessionId = getUserIdBySessionId

processOneWSMessageEcho :: (SessionRepo m, WSRepo m) => SessionId -> WS.WSMessage -> m (Maybe (WS.WSConnection, WS.WSMessage))
processOneWSMessageEcho sId wsmsg = do
  case Text.splitAt 5 wsmsg of
    ("lobb:", msg) -> pure Nothing
    ("chat:", msg) -> pure Nothing
    ("echo:", msg) -> do
      mayChan <- getWSChanBySessionId sId
      case mayChan of
        Just WSChan{wschanConn} -> pure $ Just (wschanConn, msg)
        Nothing -> pure Nothing
    _ -> do -- no prefix for active Game ws messages
      mayChan <- getWSChanBySessionId sId
      case mayChan of
        Just WSChan{wschanConn} -> pure $ Just (wschanConn, wsmsg)
        Nothing -> pure Nothing


