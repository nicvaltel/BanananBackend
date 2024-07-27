module Domain.Server 
  ( UserIdx
  , UserId(..)
  , SessionIdx
  , SessionId(..)
  , SessionIdHost
  , SessionIdGuest
  , GameRoom(..)
  , GameRoomIdx
  , GameRoomId(..)
  , SessionRepo(..)
  , WSRepo(..)
  , GameRepo(..)
  , resolveSessionId
  ) where


import Reexport
import ClassyPrelude
import Domain.Game
import qualified WebSocketServer as WS
import qualified Domain.GameBot.GameModel as G


type SessionIdx = Int
type SessionIdHost = SessionId
type SessionIdGuest = SessionId
newtype SessionId = SessionId {unSessionId :: SessionIdx}
  deriving (Show, Eq, Ord)

type UserIdx = Int
newtype UserId = UserId {unUserId :: UserIdx}
  deriving (Show, Eq, Ord)

type GameRoomIdx = Int
newtype GameRoomId = GameRoomId {unGameRoomId :: GameRoomIdx}
  deriving (Show, Eq, Ord)


data GameRoom = GameRoom
  { gameRoomGameType :: GameType
  , gameRoomHost :: SessionId
  , gameRoomGuest :: SessionId
  } deriving (Show, Eq, Ord)

class Monad m => SessionRepo m where
  initNewGuestSession :: m (SessionId, UserId)
  initKnownUserSession :: UserId -> m (Maybe (SessionId, UserId))
  restoreExistingSession :: SessionId -> UserId -> m (Maybe (SessionId, UserId))
  disconnectSession :: SessionId -> m ()
  getUserIdBySessionId :: SessionId -> m (Maybe UserId)


-- TODO Important fix: make all updating messages in WSChan via TVar, not to update atomiccally all Server every time! (as it done for serverGameStates)
class Monad m => WSRepo m where
  initWSConn :: WS.WSConnection -> SessionId -> m (Either Text ())
  disconnectWSConn :: WS.WSConnection -> SessionId -> m ()
  pushInputWSMessage :: SessionId -> WS.WSMessage -> m ()
  processWSMessages :: ([WS.WSMessage] -> WS.WSMessage -> State G.GameState [WS.WSMessage]) -> SessionId -> m () -- TODO move partially it ot game logic
  sendOutWSMessage :: SessionId -> m ()
  sendOutAllWSMessages :: m ()

class Monad m => GameRepo m where
  addGameToLobby :: SessionId -> GameType -> m ()
  startGame :: SessionIdHost -> SessionIdGuest -> GameType -> m GameRoomId


resolveSessionId :: SessionRepo m => SessionId -> m (Maybe UserId)
resolveSessionId = getUserIdBySessionId
