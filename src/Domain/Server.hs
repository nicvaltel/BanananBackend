module Domain.Server where


import Reexport
import Domain.Game
import qualified Network.WebSockets as WebSockets
import qualified Domain.GameBot.GameModel as G


type WSConnection = WebSockets.Connection

type WSMessage = Text

type UserIdx = Int

type SessionIdx = Int

data UserId = 
      RegUserId UserIdx
    | GuestUserId UserIdx
    | BotUserId UserIdx
      deriving (Show, Eq, Ord)

data SessionId =
    RegSessionId SessionIdx
  | GuestSessionId SessionIdx
  | BotSessionId SessionIdx
      deriving (Show, Eq, Ord)  

type SessionIdHost = SessionId

type SessionIdGuest = SessionId

type GameRoomId = Int

data SessionData = SessionData
  deriving (Show, Eq, Ord)

defaultSessionData :: SessionData
defaultSessionData = SessionData

data GameRoom = GameRoom
  { gameRoomGameType :: GameType
  , gameRoomHost :: SessionId
  , gameRoomGuest :: SessionId
  } deriving (Show, Eq, Ord)

class Monad m => Server m where
  initSession :: WSConnection -> UserId -> m SessionId
  disconnectSession :: SessionId -> m ()
  sendOutMessage :: SessionId -> m ()
  pushInputMessage :: SessionId -> WSMessage -> m ()
  processMessages :: ([WSMessage] -> WSMessage -> State G.GameState [WSMessage]) -> SessionId -> m ()
  addGameToLobby :: SessionId -> GameType -> m ()
  startGame :: SessionIdHost -> SessionIdGuest -> GameType -> m GameRoomId

