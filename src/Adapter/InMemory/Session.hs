{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module Adapter.InMemory.Session where


import Reexport
import Domain.User
import Domain.Session
import qualified Data.Map as Map
import Adapter.InMemory.Type
import Data.Has (Has(getter))


newGuestSession :: InMemory r m (UserId 'Guest, SessionId 'Guest)
newGuestSession = do
  tvar :: TVar Session <- asks getter
  liftIO $ do 
    sId <- stringRandomIO "[A-Za-z0-9]{16}"
    atomically $ do
      session <- readTVar tvar
      let sessionId = SessionId sId :: SessionId 'Guest
      let gId = sessionGuestIdCounter session + 1
      let guestId = UserId gId :: UserId 'Guest
      let newSession = session
            { sessionGuestIdCounter = gId
            , sessionActiveGuests = Map.insert sessionId guestId (sessionActiveGuests session)
            }
      writeTVar tvar newSession
      pure (guestId, sessionId)

newRegUserSession :: UserId 'Reg -> InMemory r m (SessionId 'Reg)
newRegUserSession userId = do
  tvar <- asks getter
  liftIO $ do 
    sId <- stringRandomIO "[A-Za-z0-9]{16}"
    atomically $ do
      session <- readTVar tvar
      let sessionId = SessionId sId :: SessionId 'Reg
      let newSession = session
            { sessionActiveRegUsers = Map.insert sessionId userId (sessionActiveRegUsers session)
            }
      writeTVar tvar newSession
      pure sessionId

findRegUserIdBySessionId :: SessionId 'Reg -> InMemory r m (Maybe (UserId 'Reg))
findRegUserIdBySessionId sessionId = do
  tvar <- asks getter
  session <- liftIO $ readTVarIO tvar
  pure $ Map.lookup sessionId (sessionActiveRegUsers session)

findGuestIdBySessionId :: SessionId 'Guest -> InMemory r m (Maybe (UserId 'Guest))
findGuestIdBySessionId sessionId = do
  tvar <- asks getter
  session <- liftIO $ readTVarIO tvar
  pure $ Map.lookup sessionId (sessionActiveGuests session)

deleteRegUserSession :: SessionId 'Reg -> InMemory r m ()
deleteRegUserSession sessionId = do
  tvar <- asks getter
  liftIO $ do 
    atomically $ do
      session <- readTVar tvar
      let newSession = session
            { sessionActiveRegUsers = Map.delete sessionId (sessionActiveRegUsers session)
            }
      writeTVar tvar newSession

deleteGuestSession :: SessionId 'Guest -> InMemory r m ()
deleteGuestSession sessionId = do
  tvar <- asks getter 
  liftIO $ do 
    atomically $ do
      session <- readTVar tvar
      let newSession = session
            { sessionActiveGuests = Map.delete sessionId (sessionActiveGuests session)
            }
      writeTVar tvar newSession
