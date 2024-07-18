{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Adapter.InMemory.WSServ where

import Reexport
import Domain.Session
import Adapter.InMemory.Type
import qualified Network.WebSockets as WS
import WebSocketServer
import qualified Data.Map as Map
import Data.Has (Has(getter))
import Domain.GameBot.GameModel (GameState, initialGameState)


sendOutMessage :: InMemory r m => WSSessionId -> m ()
sendOutMessage wsId = do
  tvar :: TVar Session <- asks getter
  mayMsg <- liftIO $ atomically $ do
    session <- readTVar tvar
    let chans = sessionWSChans session
    case Map.lookup wsId chans of
      Nothing -> pure Nothing
      Just chan -> do
        case wschanOut chan of
          [] -> pure Nothing
          msg:rest -> do
              writeTVar tvar session{sessionWSChans = Map.insert wsId chan{wschanOut = rest} chans }
              pure $ Just (msg, wschanConn chan)
  case mayMsg of
    Nothing -> pure ()
    Just (msg, conn) -> liftIO $ WS.sendTextData conn msg 

pushInputMessage :: InMemory r m => WSSessionId -> WSMessage -> m ()
pushInputMessage wsId msg = do
  tvar <- asks getter 
  liftIO $ atomically $ do
    session <- readTVar tvar
    let chans = sessionWSChans session
    case Map.lookup wsId chans of
      Nothing -> pure ()
      Just chan -> writeTVar tvar session{sessionWSChans = Map.insert wsId (pushWSIn msg chan) chans}  
      

processMessagesEcho :: InMemory r m => WSSessionId -> m ()
processMessagesEcho wsId = do
  tvar <- asks getter
  liftIO $ atomically $ do
    session <- readTVar tvar
    let chans = sessionWSChans session
    case Map.lookup wsId chans of
      Nothing -> pure ()
      Just chan@WSChan{wschanIn, wschanOut} -> do
        let newChan = chan{wschanIn = [], wschanOut = reverse wschanIn <> wschanOut }
        writeTVar tvar session
            { sessionWSChans = Map.insert wsId newChan chans
            , sessionWSToSend = wsId : sessionWSToSend session}  



processMessages :: InMemory r m => ([WSMessage] -> WSMessage -> State GameState [WSMessage]) -> WSSessionId -> m ()
processMessages processWSMsg wsId = do 
  tvar <- asks getter
  (msgs, tvarStates) <- liftIO $ atomically $ do
    session <- readTVar tvar
    let chans = sessionWSChans session
    case Map.lookup wsId chans of
      Nothing -> pure ([], sessionGameStates session)
      Just chan@WSChan{wschanIn} -> do
        let newChan = chan{wschanIn = []}
        writeTVar tvar session{sessionWSChans = Map.insert wsId newChan chans}
        pure  (wschanIn, sessionGameStates session) 

  unless (null msgs) $ do
    case Map.lookup wsId tvarStates of
      Nothing -> pure ()
      Just tvSt -> do
        outMsgs <- liftIO $ atomically $ do
          gs <- readTVar tvSt
          -- foldM :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b
          -- m :: State GameState
          -- m b :: State GameState [WSMessage]
          -- a :: WSMessage
          -- b :: [WSMessage]
          let (outMsgs, newGs) = runState (foldM processWSMsg [] (reverse msgs)) gs
          writeTVar tvSt  newGs
          pure outMsgs

        unless (null outMsgs) $ liftIO $ atomically $ do
          session <- readTVar tvar
          let chans = sessionWSChans session
          case Map.lookup wsId chans of
            Nothing -> pure ()
            Just chan@WSChan{wschanOut} -> do
              let newChan = chan{wschanOut = reverse outMsgs <> wschanOut }
              writeTVar tvar session
                  { sessionWSChans = Map.insert wsId newChan chans
                  , sessionWSToSend = wsId : sessionWSToSend session
                  }
-- processWSMsg':: WSMessage -> GameState -> (GameState, WSMessage)
-- let (newGs :: GameState, outMsgs) = foldr (\msg (gsAcc,outAcc)-> let (gs', out') = processWSMsg' msg gsAcc in (gs', out' : outAcc)) (gs,[]) msgs


initWSSession :: InMemory r m => WSConnection -> m WSSessionId
initWSSession conn = do
  tvar <- asks getter
  gameState <- liftIO $ newTVarIO initialGameState
  liftIO $ atomically $ do
    session <- readTVar tvar
    let wsId = sessionWSSessionIdCounter session + 1
    let wsSessionId = WSSessionId wsId
    let newSession = session 
          { sessionWSSessionIdCounter = wsId
          , sessionWSChans = Map.insert wsSessionId (emptyWSChan conn) (sessionWSChans session)
          , sessionGameStates = Map.insert wsSessionId gameState (sessionGameStates session) 
          }
    writeTVar tvar newSession
    pure wsSessionId


disconnectWSSession :: InMemory r m => WSSessionId -> m ()
disconnectWSSession wsId = do
  tvar <- asks getter
  liftIO $ atomically $ do
    session <- readTVar tvar
    let newSession = session
          { sessionWSChans = Map.delete wsId (sessionWSChans session)
          , sessionGameStates = Map.delete wsId (sessionGameStates session)
          }
    writeTVar tvar newSession