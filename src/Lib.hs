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
  newSession = Mem.newSession
  findUserBySessionId = Mem.findUserBySessionId

instance WSRepo App where
  -- initWSConn = Mem.initWSConn
  disconnectWSConn = Mem.disconnectWSConn
  -- pushInputWSMessage = undefined -- Mem.pushInputWSMessage
  -- processWSMessages = undefined -- Mem.processWSMessages
  -- sendOutWSMessage = undefined -- Mem.sendOutWSMessage
  -- sendOutAllWSMessages = undefined -- Mem.sendOutAllWSMessages
  -- getWSChanBySessionId = undefined -- Mem.getWSChanBySessionId

instance GameRepo App where
  addGameToLobby = Mem.addGameToLobby
  getLobbyEntries = Mem.getLobbyEntries
  joinGame = Mem.joinGame
  -- startGameWithBot = undefined -- Mem.startGameWithBot


runSession :: AppState -> App a -> IO a
runSession state = flip runReaderT state . unApp
  
runRoutine :: App () -> IO ()
runRoutine routine = do 
  ss <- newTVarIO Mem.initialServerState
  runSession ss routine
  pure ()
