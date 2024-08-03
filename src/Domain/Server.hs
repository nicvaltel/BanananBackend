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
  ) where


import Reexport
import ClassyPrelude
import Domain.Game
import qualified WebSocketServer as WS
import qualified Domain.GameBot.GameModel as G


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
  initWSConn :: WS.WSConnection -> SessionId -> m (Either SessionError ())
  disconnectWSConn :: WS.WSConnection -> SessionId -> m ()
  pushInputWSMessage :: SessionId -> WS.WSMessage -> m ()
  processWSMessages :: (SessionId -> WS.WSMessage -> WS.WSMessage) -> SessionId -> m () -- TODO move partially it ot game logic
  sendOutWSMessage :: SessionId -> m ()
  sendOutAllWSMessages :: m ()

class Monad m => GameRepo m where
  addGameToLobby :: SessionId -> GameType -> m (Either LobbyError LobbyId)
  joinGame :: SessionIdGuest -> LobbyId -> m (Maybe GameRoomId)
  startGameWithBot :: SessionId -> GameType -> m (Either LobbyError GameRoomId)

resolveSessionId :: SessionRepo m => SessionId -> m (Maybe UserId)
resolveSessionId = getUserIdBySessionId
