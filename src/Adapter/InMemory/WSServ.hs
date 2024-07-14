{-# LANGUAGE RankNTypes #-}
module Adapter.InMemory.WSServ where

import Reexport
import Domain.Session
import Adapter.InMemory.Type
import qualified Network.WebSockets as WS
import WebSocketServer
import qualified Data.Map as Map
import Control.Monad (foldM)


sendOutMessage :: WSSessionId -> InMemory gs m ()
sendOutMessage wsId = do
  tvar <- ask
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

pushInputMessage :: WSSessionId -> WSMessage -> InMemory gs m ()
pushInputMessage wsId msg = do
  tvar <- ask
  liftIO $ atomically $ do
    session <- readTVar tvar
    let chans = sessionWSChans session
    case Map.lookup wsId chans of
      Nothing -> pure ()
      Just chan -> writeTVar tvar session{sessionWSChans = Map.insert wsId (pushWSIn msg chan) chans}  
      

processMessages :: Bot m => WSSessionId -> InMemory gs m ()
processMessages wsId = do
  tvar <- ask
  mayGsMsgs <- liftIO $ atomically $ do
    session <- readTVar tvar
    let chans = sessionWSChans session
    case Map.lookup wsId chans of
      Nothing -> pure Nothing
      Just chan@WSChan{wschanIn} -> do
        case Map.lookup wsId (sessionGameStates session) of
          Nothing -> pure Nothing
          Just gs -> do
            let newChan = chan{wschanIn = [] }
            writeTVar tvar session
                { sessionWSChans = Map.insert wsId newChan chans}
            pure $ Just (gs, wschanIn) 
  case mayGsMsgs of
    Nothing -> pure ()
    Just (gs, inputMsgs) -> do
      (newGs, outMsgs) <- lift $ foldM processFoldM (gs,[]) (reverse inputMsgs)
        -- foldM 
        --   (\(gsAcc, outAcc) msg -> processWSMessage msg gsAcc >>= \(gs', out') -> pure (gs', out' : outAcc)) 
        --   (gs, []) 
        --   (reverse inputMsgs)
      liftIO $ atomically $ do
        session <- readTVar tvar
        let chans = sessionWSChans session
        case Map.lookup wsId chans of
          Nothing -> pure ()
          Just chan@WSChan{wschanOut} -> do
            let sNewGS = Map.insert wsId newGs (sessionGameStates session)
            let newChan = chan{wschanOut = reverse outMsgs <> wschanOut }
            writeTVar tvar session
                { sessionWSChans = Map.insert wsId newChan chans
                , sessionWSToSend = wsId : sessionWSToSend session
                , sessionGameStates = sNewGS
                }
  where 
    processFoldM :: Bot m => (gs, [WSMessage]) -> WSMessage -> m (gs, [WSMessage])
    processFoldM (gsAcc, outAcc) msg = do
      (gs', out') <- processWSMessage msg gsAcc
      pure (gs', out' : outAcc)

initWSSession :: WSConnection -> InMemory gs m WSSessionId
initWSSession conn = do
  tvar <- ask
  liftIO $ atomically $ do
    session <- readTVar tvar
    let wsId = sessionWSSessionIdCounter session + 1
    let wsSessionId = WSSessionId wsId
    let newSession = session 
          { sessionWSSessionIdCounter = wsId
          , sessionWSChans = Map.insert wsSessionId (emptyWSChan conn) (sessionWSChans session)  
          }
    writeTVar tvar newSession
    pure wsSessionId


disconnectWSSession :: WSSessionId -> InMemory gs m ()
disconnectWSSession wsId = do
  tvar <- ask
  liftIO $ atomically $ do
    session <- readTVar tvar
    let newSession = session
          { sessionWSChans = Map.delete wsId (sessionWSChans session)
          }
    writeTVar tvar newSession