{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
module Domain.Session 
  -- ( SessionUserId(..)
  -- , SessionGuestId(..)
  -- , WSSessionId(..)
  -- , Session (..)
  -- , WSServ(..)
  -- , initialSession
  -- , SessionRepo(..)
  -- ) 
  where


import Reexport
import Domain.User
import WebSocketServer
import Domain.GameBot.GameModel (GameState)


newtype SessionId (u :: UserKind) = SessionId Text 
  deriving (Show, Eq, Ord)
newtype WSSessionId = WSSessionId Int deriving (Show, Eq, Ord)


data Session = Session  
  { sessionAllGuest :: Set (UserId 'Guest)
  , sessionAllRegUsers :: Set (UserId 'Reg)
  , sessionActiveGuests :: Map (SessionId 'Guest) (UserId 'Guest)
  , sessionActiveRegUsers :: Map (SessionId 'Reg) (UserId 'Reg) 
  , sessionActiveBots :: Map (SessionId 'Bot) (UserId 'Bot)
  , sessionGuestIdCounter :: Int
  , sessionUserIdCounter :: Int
  , sessionBotIdCounter :: Int
  , sessionWSSessionIdCounter :: Int
  , sessionWSToSend :: [WSSessionId]
  , sessionWSChans :: Map WSSessionId WSChan
  , sessionGameStates :: Map WSSessionId (TVar GameState)
  } 

initialSession :: Session
initialSession = Session
  { sessionAllGuest = mempty
  , sessionAllRegUsers = mempty
  , sessionActiveGuests = mempty
  , sessionActiveRegUsers = mempty
  , sessionActiveBots = mempty
  , sessionGuestIdCounter = 0
  , sessionUserIdCounter = 0
  , sessionWSSessionIdCounter = 0
  , sessionBotIdCounter = 0
  , sessionWSToSend = mempty
  , sessionWSChans = mempty
  , sessionGameStates = mempty
  }


class Monad m => WSServ m where
  initWSSession :: WSConnection -> m WSSessionId
  disconnectWSSession :: WSSessionId -> m ()
  sendOutMessage :: WSSessionId -> m ()
  pushInputMessage :: WSSessionId -> WSMessage -> m ()
  processMessages :: ([WSMessage] -> WSMessage -> State GameState [WSMessage]) -> WSSessionId -> m ()

class Monad m => SessionRepo m where
  newGuestSession :: m (UserId 'Guest, SessionId 'Guest)
  newRegUserSession :: UserId 'Reg -> m (SessionId 'Reg)
  findRegUserIdBySessionId :: SessionId 'Reg -> m (Maybe (UserId 'Reg))
  findGuestIdBySessionId :: SessionId 'Guest -> m (Maybe (UserId 'Guest))
  deleteRegUserSession :: SessionId 'Reg -> m ()
  deleteGuestSession :: SessionId 'Guest -> m ()

-- -- class Monad m => Bot m where
-- --   processWSMessage :: m ([WSMessage] -> WSMessage -> State GameState [WSMessage])

-- -- instance Bot IO where  
-- --   processWSMessage = pure undefined

-- data GameRoom = GameRoom
--   { gameRoomHost :: (AnySessionId, AnyUserId)
--   , gameRoomGuest :: Int
--   }