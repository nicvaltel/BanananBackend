{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib 
  ( App(..)
  , AppState
  , runSession
  , runRoutine
  ) where

import Reexport
import ClassyPrelude
import Domain.Server
import qualified Adapter.InMemory.Server as Mem


type AppState = TVar Mem.ServerState

newtype App a = App { unApp :: ReaderT AppState IO a  } 
  deriving (Functor, Applicative, Monad, MonadReader AppState, MonadIO, MonadFail, MonadUnliftIO)



instance SessionRepo App where
  initNewGuestSession = Mem.initNewGuestSession
  initKnownUserSession = Mem.initKnownUserSession
  restoreExistingSession = Mem.restoreExistingSession
  disconnectSession = Mem.disconnectSession
  getUserIdBySessionId = Mem.getUserIdBySessionId

instance WSRepo App where
  initWSConn = Mem.initWSConn
  pushInputWSMessage = Mem.pushInputWSMessage
  processWSMessages = Mem.processWSMessages
  sendOutWSMessage = Mem.sendOutWSMessage
  sendOutAllWSMessages = Mem.sendOutAllWSMessages

instance GameRepo App where
  addGameToLobby = undefined
  startGame = undefined


runSession :: AppState -> App a -> IO a
runSession state = flip runReaderT state . unApp
  
runRoutine :: App () -> IO ()
runRoutine routine = do 
  ss <- newTVarIO Mem.initialServerState
  runSession ss routine
  pure ()
