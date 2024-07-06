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


newtype SessionUserId = SessionUserId Text deriving (Show, Eq, Ord)
newtype SessionGuestId = SessionGuestId Text deriving (Show, Eq, Ord)
newtype WSSessionId = WSSessionId Int deriving (Show, Eq, Ord)


data Session = Session  
  { sessionAllGuest :: Set GuestId
  , sessionAllUsers :: Set UserId
  , sessionActiveGuests :: Map SessionGuestId GuestId
  , sessionActiveUsers :: Map SessionUserId UserId 
  , sessionGuestIdCounter :: Int
  , sessionUserIdCounter :: Int
  , sessionWSSessionIdCounter :: Int
  , sessionWSToSend :: [WSSessionId]
  , sessionWSChans :: Map WSSessionId WSChan
  }  deriving (Show)

initialSession :: Session
initialSession = Session
  { sessionAllGuest = mempty
  , sessionAllUsers = mempty
  , sessionActiveGuests = mempty
  , sessionActiveUsers = mempty
  , sessionGuestIdCounter = 0
  , sessionUserIdCounter = 0
  , sessionWSSessionIdCounter = 0
  , sessionWSToSend = mempty
  , sessionWSChans = mempty
  }


class Monad m => WSServ m where
  initWSSession :: WSConnection -> m WSSessionId
  sendOutMessage :: WSSessionId -> m ()
  pushInputMessage :: WSSessionId -> WSMessage -> m ()
  processMessages :: WSSessionId -> m ()

class Monad m => SessionRepo m where
  newGuestSession :: m (GuestId, SessionGuestId)
  newUserSession :: UserId -> m SessionUserId
  findUserIdBySessionId :: SessionUserId -> m (Maybe UserId)
  findGuestIdBySessionId :: SessionGuestId -> m (Maybe GuestId)
  deleteUserSession :: SessionUserId -> m ()
  deleteGuestSession :: SessionGuestId -> m ()