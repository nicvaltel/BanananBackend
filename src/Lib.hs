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
  initSession = Mem.initSession
  initGuestSession = Mem.initGuestSession
  disconnectSession = Mem.disconnectSession
  sendOutMessage = Mem.sendOutMessage
  pushInputMessage = Mem.pushInputMessage
  processMessages = Mem.processMessages
  sendOutAllMessages = Mem.sendOutAllMessages
  findUserIdBySessionId _ = pure Nothing -- TODO fix it 

instance GameRepo App where
  addGameToLobby = Mem.addGameToLobby
  startGame = Mem.startGame


runSession :: AppState -> App a -> IO a
runSession state = flip runReaderT state . unApp
  
runRoutine :: App () -> IO ()
runRoutine routine = do 
  ss <- newTVarIO Mem.initialServerState
  runSession ss routine
  pure ()
