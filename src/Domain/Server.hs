module Domain.Server 
  ( WSConnection
  , WSMessage
  , UserIdx
  , UserId(..)
  , SessionIdx
  , SessionId(..)
  , SessionIdHost
  , SessionIdGuest
  , GameRoom(..)
  , GameRoomIdx
  , GameRoomId(..)
  , SessionData(..)
  , SessionRepo(..)
  , GameRepo(..)
  , defaultSessionData
  , resolveSessionId
  ) where


import Reexport
import Domain.Game
import qualified Network.WebSockets as WebSockets
import qualified Domain.GameBot.GameModel as G


type WSConnection = WebSockets.Connection

type WSMessage = Text

type UserIdx = Int

type SessionIdx = Int
newtype SessionId = SessionId {unSessionId :: SessionIdx}
  deriving (Show, Eq, Ord)

data UserId = 
      RegUserId UserIdx
    | GuestUserId UserIdx
    | BotUserId UserIdx
      deriving (Show, Eq, Ord)

type SessionIdHost = SessionId

type SessionIdGuest = SessionId

type GameRoomIdx = Int
newtype GameRoomId = GameRoomId {unGameRoomId :: GameRoomIdx}
  deriving (Show, Eq, Ord)

data SessionData = SessionData {
    sessionDataUserId :: UserId
} deriving (Show, Eq, Ord)

defaultSessionData :: UserId -> SessionData 
defaultSessionData userId = SessionData {
  sessionDataUserId = userId
}

data GameRoom = GameRoom
  { gameRoomGameType :: GameType
  , gameRoomHost :: SessionId
  , gameRoomGuest :: SessionId
  } deriving (Show, Eq, Ord)

class Monad m => SessionRepo m where
  initSession :: WSConnection -> UserId -> m SessionId
  initGuestSession :: WSConnection -> m SessionId
  disconnectSession :: SessionId -> m ()
  pushInputMessage :: SessionId -> WSMessage -> m ()
  processMessages :: ([WSMessage] -> WSMessage -> State G.GameState [WSMessage]) -> SessionId -> m ()
  sendOutMessage :: SessionId -> m ()
  sendOutAllMessages :: m ()
  findUserIdBySessionId :: SessionId -> m (Maybe UserId)

class Monad m => GameRepo m where
  addGameToLobby :: SessionId -> GameType -> m ()
  startGame :: SessionIdHost -> SessionIdGuest -> GameType -> m GameRoomId


resolveSessionId :: SessionRepo m => SessionId -> m (Maybe UserId)
resolveSessionId = findUserIdBySessionId
