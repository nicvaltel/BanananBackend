{-# LANGUAGE RankNTypes #-}
module Adapter.InMemory.WSServ where

import Reexport
import Domain.Session
import Adapter.InMemory.Type
import qualified Network.WebSockets as WS
import WebSocketServer
import qualified Data.Map as Map


sendOutMessage :: WSSessionId -> InMemory m ()
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

pushInputMessage :: WSSessionId -> WSMessage -> InMemory m ()
pushInputMessage wsId msg = do
  tvar <- ask
  liftIO $ atomically $ do
    session <- readTVar tvar
    let chans = sessionWSChans session
    case Map.lookup wsId chans of
      Nothing -> pure ()
      Just chan -> writeTVar tvar session{sessionWSChans = Map.insert wsId (pushWSIn msg chan) chans}  
      

processMessages :: WSSessionId -> InMemory m ()
processMessages wsId = do
  tvar <- ask
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



initWSSession :: WSConnection -> InMemory m WSSessionId
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