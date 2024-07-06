{-# LANGUAGE RankNTypes #-}

module Adapter.InMemory.Session where


import Reexport
import Domain.User
import Domain.Session
import qualified Data.Map as Map
import Adapter.InMemory.Type


newGuestSession :: InMemory m (GuestId, SessionGuestId)
newGuestSession = do
  tvar <- ask
  liftIO $ do 
    sId <- stringRandomIO "[A-Za-z0-9]{16}"
    atomically $ do
      session <- readTVar tvar
      let sessionId = SessionGuestId sId
      let gId = sessionGuestIdCounter session + 1
      let guestId = GuestId gId
      let newSession = session
            { sessionGuestIdCounter = gId
            , sessionActiveGuests = Map.insert sessionId guestId (sessionActiveGuests session)
            }
      writeTVar tvar newSession
      pure (guestId, sessionId)

newUserSession :: UserId -> InMemory m SessionUserId
newUserSession userId = do
  tvar <- ask
  liftIO $ do 
    sId <- stringRandomIO "[A-Za-z0-9]{16}"
    atomically $ do
      session <- readTVar tvar
      let sessionId = SessionUserId sId
      let newSession = session
            { sessionActiveUsers = Map.insert sessionId userId (sessionActiveUsers session)
            }
      writeTVar tvar newSession
      pure sessionId

findUserIdBySessionId :: SessionUserId -> InMemory m (Maybe UserId)
findUserIdBySessionId sessionId = do
  tvar <- ask
  session <- liftIO $ readTVarIO tvar
  pure $ Map.lookup sessionId (sessionActiveUsers session)

findGuestIdBySessionId :: SessionGuestId -> InMemory m (Maybe GuestId)
findGuestIdBySessionId sessionId = do
  tvar <- ask
  session <- liftIO $ readTVarIO tvar
  pure $ Map.lookup sessionId (sessionActiveGuests session)

deleteUserSession :: SessionUserId -> InMemory m ()
deleteUserSession sessionId = do
  tvar <- ask
  liftIO $ do 
    atomically $ do
      session <- readTVar tvar
      let newSession = session
            { sessionActiveUsers = Map.delete sessionId (sessionActiveUsers session)
            }
      writeTVar tvar newSession

deleteGuestSession :: SessionGuestId -> InMemory m ()
deleteGuestSession sessionId = do
  tvar <- ask
  liftIO $ do 
    atomically $ do
      session <- readTVar tvar
      let newSession = session
            { sessionActiveGuests = Map.delete sessionId (sessionActiveGuests session)
            }
      writeTVar tvar newSession
