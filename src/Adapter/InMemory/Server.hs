{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Adapter.InMemory.Server 
  ( ServerState
  , initialServerState
  , initSession
  , disconnectSession
  , addGameToLobby
  , startGame
  , sendOutMessage
  , pushInputMessage
  , processMessages
  , sendOutAllMessages
  , initGuestSession
  , findSessionDataBySessionId
  ) where

import Reexport
import ClassyPrelude

import qualified Domain.Server as D

import Domain.Game (GameType)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified WebSocketServer as WS 
import WebSocketServer(WSMessage, WSConnection, WSChan(..))
import qualified Domain.GameBot.GameModel as G


type InMemory r m = (Has (TVar ServerState) r, MonadReader r m, MonadIO m)

data ServerState = ServerState {
    serverSessions :: Map D.SessionId D.SessionData
  , serverLobby :: Map GameType (Set D.SessionId)
  , serverActiveGames :: Map D.GameRoomId D.GameRoom
  , serverUserIdCounter :: D.UserIdx
  , serverSessionIdCounter :: D.SessionIdx -- common for all UserTypes
  , serverGameRoomIdCounter :: D.GameRoomIdx
  , serverWSToSend :: [D.SessionId]
  , serverWSChans :: Map D.SessionId WS.WSChan
  , serverGameStates :: Map D.SessionId (TVar G.GameState)
} 


initialServerState :: ServerState
initialServerState = ServerState {
    serverSessions = mempty
  , serverLobby = mempty
  , serverActiveGames = mempty
  , serverUserIdCounter = 0
  , serverSessionIdCounter = 0
  , serverGameRoomIdCounter = 0
  , serverWSToSend = mempty
  , serverWSChans = mempty
  , serverGameStates = mempty
}

initGuestSession :: InMemory r m => WSConnection -> m D.SessionId
initGuestSession conn = do
  tvar <- asks getter
  gameState <- liftIO $ newTVarIO G.initialGameState
  liftIO $ atomically $ do
    ss <- readTVar tvar
    let uId = serverUserIdCounter ss + 1
    let userId = D.UserId uId
    let sId = serverSessionIdCounter ss + 1
    let sessionId = D.SessionId sId
    let newSessions = Map.insert sessionId (D.defaultSessionData userId) (serverSessions ss)
    let newWSChans = Map.insert sessionId (WS.emptyWSChan conn) (serverWSChans ss)
    let newGameStates = Map.insert sessionId gameState (serverGameStates ss)
    let newSession = ss 
          { serverUserIdCounter = uId 
          , serverSessionIdCounter = sId
          , serverSessions = newSessions
          , serverWSChans = newWSChans
          , serverGameStates = newGameStates 
          }
    writeTVar tvar newSession
    pure sessionId

initSession :: InMemory r m => WSConnection -> D.UserId -> m D.SessionId
initSession conn userId = do
  tvar <- asks getter
  gameState <- liftIO $ newTVarIO G.initialGameState
  liftIO $ atomically $ do
    ss <- readTVar tvar
    let sId = serverSessionIdCounter ss + 1
    let sessionId = D.SessionId sId
    let newSessions = Map.insert sessionId (D.defaultSessionData userId) (serverSessions ss)
    let newWSChans = Map.insert sessionId (WS.emptyWSChan conn) (serverWSChans ss)
    let newGameStates = Map.insert sessionId gameState (serverGameStates ss)
    let newSession = ss 
          { serverSessionIdCounter = sId
          , serverSessions = newSessions
          , serverWSChans = newWSChans
          , serverGameStates = newGameStates 
          }
    writeTVar tvar newSession
    pure sessionId


disconnectSession :: InMemory r m => D.SessionId -> m ()
disconnectSession sId = do
  tvar <- asks getter
  liftIO $ atomically $ do
    ss <- readTVar tvar
    let newServer = ss{
            serverSessions = Map.delete sId (serverSessions ss)
          , serverWSChans = Map.delete sId (serverWSChans ss)
          , serverGameStates = Map.delete sId (serverGameStates ss)
          }
    writeTVar tvar newServer


-- TODO check there is no active game for this sId
addGameToLobby :: InMemory r m => D.SessionId -> GameType -> m ()
addGameToLobby sId gameType = do
  tvar <- asks getter
  liftIO $ atomically $ do
    ss :: ServerState <- readTVar tvar
    let newLobby = Map.update (Just . Set.insert sId) gameType (serverLobby ss)
    writeTVar tvar ss{serverLobby = newLobby}


startGame :: InMemory r m => D.SessionIdHost -> D.SessionIdGuest -> GameType -> m D.GameRoomId
startGame sHostId sGuestId gameType = do
  tvar <- asks getter
  let newGameRoom = D.GameRoom
        { D.gameRoomGameType = gameType
        , D.gameRoomHost = sHostId
        , D.gameRoomGuest = sGuestId
        }
  liftIO $ atomically $ do
    ss :: ServerState <- readTVar tvar
    let grIdx = serverGameRoomIdCounter ss + 1
    let gameRoomId = D.GameRoomId grIdx
    let newLobby = Map.update (Just . Set.delete sHostId ) gameType (serverLobby ss)
    let newActiveGames = Map.insert gameRoomId newGameRoom (serverActiveGames ss)
    writeTVar tvar ss{serverLobby = newLobby, serverActiveGames = newActiveGames, serverGameRoomIdCounter = grIdx}
    pure gameRoomId


sendOutMessage :: InMemory r m => D.SessionId -> m ()
sendOutMessage sId = do
  tvar :: TVar ServerState <- asks getter
  mayMsg <- liftIO $ atomically $ do
    ss <- readTVar tvar
    let chans = serverWSChans ss
    case Map.lookup sId chans of
      Nothing -> pure Nothing
      Just chan -> do
        case wschanOut chan of
          [] -> pure Nothing
          msg:rest -> do
              writeTVar tvar ss{
                  serverWSChans = Map.insert sId chan{wschanOut = rest} chans
                }
              pure $ Just (msg, wschanConn chan)
  case mayMsg of
    Nothing -> pure ()
    Just (msg, conn) -> liftIO $ WS.sendTextData conn msg 



pushInputMessage :: InMemory r m => D.SessionId -> WSMessage -> m ()
pushInputMessage sId msg = do
  tvar <- asks getter 
  liftIO $ atomically $ do
    ss <- readTVar tvar
    let chans = serverWSChans ss
    case Map.lookup sId chans of
      Nothing -> pure ()
      Just chan -> writeTVar tvar ss{
          serverWSChans = Map.insert sId (WS.pushWSIn msg chan) chans}  
      

processMessagesEcho :: InMemory r m => D.SessionId -> m ()
processMessagesEcho sId = do
  tvar <- asks getter
  liftIO $ atomically $ do
    ss <- readTVar tvar
    let chans = serverWSChans ss
    case Map.lookup sId chans of
      Nothing -> pure ()
      Just chan@WSChan{wschanIn, wschanOut} -> do
        let newChan = chan{wschanIn = [], wschanOut = reverse wschanIn <> wschanOut }
        writeTVar tvar ss{ 
              serverWSChans = Map.insert sId newChan chans
            , serverWSToSend = sId : serverWSToSend ss}  



processMessages :: InMemory r m => ([WSMessage] -> WSMessage -> State G.GameState [WSMessage]) -> D.SessionId -> m ()
processMessages processWSMsg sId = do 
  tvar <- asks getter
  (msgs, tvarStates) <- liftIO $ atomically $ do
    ss <- readTVar tvar
    let chans = serverWSChans ss
    case Map.lookup sId chans of
      Nothing -> pure ([], serverGameStates ss)
      Just chan@WSChan{wschanIn} -> do
        let newChan = chan{wschanIn = []}
        writeTVar tvar ss{serverWSChans = Map.insert sId newChan chans}
        pure  (wschanIn, serverGameStates ss) 

  unless (null msgs) $ do
    case Map.lookup sId tvarStates of
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
          let chans = serverWSChans session
          case Map.lookup sId chans of
            Nothing -> pure ()
            Just chan@WSChan{wschanOut} -> do
              let newChan = chan{wschanOut = reverse outMsgs <> wschanOut }
              writeTVar tvar session
                  { serverWSChans = Map.insert sId newChan chans
                  , serverWSToSend = sId : serverWSToSend session
                  }

sendOutAllMessages :: InMemory r m => m ()
sendOutAllMessages = do
  tvar <- asks getter
  sIds <- liftIO $ atomically $ do
    ss <- readTVar tvar
    let ids = serverWSToSend ss
    writeTVar tvar ss{serverWSToSend = []}
    pure ids
  traverse_ sendOutMessage sIds


findSessionDataBySessionId :: InMemory r m => D.SessionId -> m (Maybe D.SessionData)
findSessionDataBySessionId sessionId = do
  tvar <- asks getter
  ss <- liftIO $ readTVarIO tvar
  let maySessionData = Map.lookup sessionId (serverSessions ss)
  pure maySessionData