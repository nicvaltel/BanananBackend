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
import Domain.Server
import qualified Adapter.InMemory.Server as Mem


type AppState = TVar Mem.ServerState

newtype App r a = App { unApp :: ReaderT AppState IO a  } 
  deriving (Functor, Applicative, Monad, MonadReader AppState, MonadIO, MonadFail)


instance ServerRepo (App AppState) where
  initSession = Mem.initSession
  disconnectSession = Mem.disconnectSession
  sendOutMessage = Mem.sendOutMessage
  pushInputMessage = Mem.pushInputMessage
  processMessages = Mem.processMessages
  sendOutAllMessages = Mem.sendOutAllMessages

instance GameRepo (App AppState) where
  addGameToLobby = Mem.addGameToLobby
  startGame = Mem.startGame


runSession :: AppState -> App AppState a -> IO a
runSession state = flip runReaderT state . unApp
  
runRoutine :: App AppState () -> IO ()
runRoutine routine = do 
  ss <- newTVarIO Mem.initialServerState
  runSession ss routine
  pure ()
