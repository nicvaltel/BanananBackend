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
import Control.Monad (foldM)
import Data.Has (Has(getter))
import Domain.GameBot.GameModel (GameState (GameState))


sendOutMessage :: WSSessionId -> InMemory r m ()
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

pushInputMessage :: WSSessionId -> WSMessage -> InMemory r m ()
pushInputMessage wsId msg = do
  tvar <- asks getter 
  liftIO $ atomically $ do
    session <- readTVar tvar
    let chans = sessionWSChans session
    case Map.lookup wsId chans of
      Nothing -> pure ()
      Just chan -> writeTVar tvar session{sessionWSChans = Map.insert wsId (pushWSIn msg chan) chans}  
      

processMessages' :: WSSessionId -> InMemory r m ()
processMessages' wsId = do
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



processMessages :: (WSMessage -> GameState -> (GameState, WSMessage)) -> WSSessionId -> InMemory r m ()
processMessages processWSMsg wsId = do (processMessages' wsId)
  -- tvar <- asks getter
  -- tvarStates :: Map WSSessionId (TVar GameState) <- asks getter
  -- case Map.lookup wsId tvarStates of
  --   Nothing -> pure ()
  --   Just tvSt -> do
  --     msgs <- liftIO $ atomically $ do
  --       session <- readTVar tvar
  --       let chans = sessionWSChans session
  --       case Map.lookup wsId chans of
  --         Nothing -> pure []
  --         Just chan@WSChan{wschanIn} -> do
  --           let newChan = chan{wschanIn = [] }
  --           writeTVar tvar session
  --               { sessionWSChans = Map.insert wsId newChan chans}
  --           pure  wschanIn 
  --     unless (null msgs) $ liftIO $ atomically $ do
  --       gs <- readTVar tvSt
  --       let (newGs :: GameState, outMsgs) = foldr 
  --               (\msg (gsAcc,outAcc)-> let (gs', out') = processWSMsg msg gsAcc in (gs', out' : outAcc)) 
  --               (gs,[]) 
  --               msgs 
  --       session <- readTVar tvar
  --       let chans = sessionWSChans session
  --       case Map.lookup wsId chans of
  --         Nothing -> pure ()
  --         Just chan@WSChan{wschanOut} -> do
  --           let newChan = chan{wschanOut = reverse outMsgs <> wschanOut }
  --           writeTVar tvar session
  --               { sessionWSChans = Map.insert wsId newChan chans
  --               , sessionWSToSend = wsId : sessionWSToSend session
  --               }
  --           writeTVar tvSt  newGs


-- foldM 
--   (\(gsAcc, outAcc) msg -> processWSMessage msg gsAcc >>= \(gs', out') -> pure (gs', out' : outAcc)) 
--   (gs, []) 
--   (reverse inputMsgs)


initWSSession :: WSConnection -> InMemory r m WSSessionId
initWSSession conn = do
  tvar <- asks getter
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


disconnectWSSession :: WSSessionId -> InMemory r m ()
disconnectWSSession wsId = do
  tvar <- asks getter
  liftIO $ atomically $ do
    session <- readTVar tvar
    let newSession = session
          { sessionWSChans = Map.delete wsId (sessionWSChans session)
          }
    writeTVar tvar newSession