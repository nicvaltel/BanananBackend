{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}

module Adapter.InMemory.Session where


import Reexport
import Domain.User
import qualified Data.Map as Map
import Adapter.InMemory.Type
import Domain.Session
import Data.Has (Has(..))


newGuestSession :: InMemory r m => m (UserId 'Guest, SessionId 'Guest)
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

newRegUserSession :: InMemory r m => UserId 'Reg -> m (SessionId 'Reg)
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

findRegUserIdBySessionId :: InMemory r m => SessionId 'Reg -> m (Maybe (UserId 'Reg))
findRegUserIdBySessionId sessionId = do
  tvar <- asks getter
  session <- liftIO $ readTVarIO tvar
  pure $ Map.lookup sessionId (sessionActiveRegUsers session)

findGuestIdBySessionId :: InMemory r m => SessionId 'Guest -> m (Maybe (UserId 'Guest))
findGuestIdBySessionId sessionId = do
  tvar <- asks getter
  session <- liftIO $ readTVarIO tvar
  pure $ Map.lookup sessionId (sessionActiveGuests session)

deleteRegUserSession :: InMemory r m => SessionId 'Reg -> m ()
deleteRegUserSession sessionId = do
  tvar <- asks getter
  liftIO $ do 
    atomically $ do
      session <- readTVar tvar
      let newSession = session
            { sessionActiveRegUsers = Map.delete sessionId (sessionActiveRegUsers session)
            }
      writeTVar tvar newSession

deleteGuestSession :: InMemory r m => SessionId 'Guest -> m ()
deleteGuestSession sessionId = do
  tvar <- asks getter 
  liftIO $ do 
    atomically $ do
      session <- readTVar tvar
      let newSession = session
            { sessionActiveGuests = Map.delete sessionId (sessionActiveGuests session)
            }
      writeTVar tvar newSession
