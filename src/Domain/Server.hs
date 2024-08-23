{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

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
  , SessionData(..)
  , sdMayUserId
  , sdWSConn
  , sdWSChan
  , sdMayActiveGame
  , generateNewSessionId
  , resolveSessionData
  -- , processOneWSMessageEcho
  , Token
  -- , initGuestSession
  -- , initRegUserSession
  , lengthOfSessionIdConst
  , checkSessionGameStatus
  , checkGameInLobby
  ) where


import ClassyPrelude
import Domain.Game
import qualified WebSocketServer as WS
import WebSocketServer(WSMessage, WSChan(..), WSConnection)
import qualified Domain.GameBot.GameModel as G
import qualified Data.Text as Text
import Text.StringRandom (stringRandomIO)
import Control.Lens

type Token = Text
type SessionIdHost = SessionId
type SessionIdGuest = SessionId

newtype SessionId = SessionId {unSessionId :: Text}
  deriving (Show, Eq, Ord)

lengthOfSessionIdConst :: Int
lengthOfSessionIdConst = 32

data SessionData = SessionData
  { _sdMayUserId :: Maybe UserId -- Nothing for guest
  , _sdWSConn :: WSConnection
  , _sdWSChan :: WS.WSChan
  , _sdMayActiveGame :: Maybe GameRoomId
  }

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
  | LobbyErrorSessionIsNotActive
    deriving (Show, Eq, Ord)

data SessionError =
    SessionErrorSessionIdIsNotActive
  | SessionErrorGameRoomIdIsNotActive
  | SessionErrorWSConnectionIsAlreadyActive
    deriving(Show, Eq, Ord)

makeLenses ''SessionData


class Monad m => SessionRepo m where
  newSession :: Maybe UserId -> SessionId -> WSConnection -> m (Either SessionError WSChan)
  findUserBySessionId :: SessionId -> m (Maybe SessionData)

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

resolveSessionData :: SessionRepo m => SessionId -> m (Maybe SessionData)
resolveSessionData = findUserBySessionId

-- checkLobbyGameStatus :: GameRepo m => LobbyId -> m (Maybe GameRoomId)
-- checkLobbyGameStatus lbId = do
--   lobbys <- getLobbyEntries
--   case filter ((==) lbId . lobbyLobbyId) lobbys of
--     [lobby] -> pure (lobbyMaybeGameRoomId lobby)
--     _ -> pure Nothing

checkSessionGameStatus :: SessionRepo m => SessionId -> m (Maybe GameRoomId)
checkSessionGameStatus sId = do
  maySd <- resolveSessionData sId
  case maySd of
    Nothing -> pure Nothing
    Just sd -> pure (sd ^. sdMayActiveGame)


checkGameInLobby :: GameRepo m => LobbyId -> m Bool
checkGameInLobby lbId = any ((==) lbId . lobbyLobbyId) <$> getLobbyEntries


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
        



