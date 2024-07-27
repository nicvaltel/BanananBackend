{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Adapter.InMemory.Server 
  ( ServerState
  , initialServerState
  , initNewGuestSession
  , initKnownUserSession
  , restoreExistingSession
  , disconnectSession
  , getUserIdBySessionId
  , initWSConn
  , pushInputWSMessage
  , processWSMessagesEcho
  , processWSMessages
  , sendOutWSMessage
  , sendOutAllWSMessages
  , disconnectWSConn
  ) where

import Reexport
import ClassyPrelude
import qualified Domain.Server as D
import Domain.Game (GameType)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified WebSocketServer as WS 
import WebSocketServer(WSMessage, WSChan(..))
import qualified Domain.GameBot.GameModel as G


type InMemory r m = (Has (TVar ServerState) r, MonadReader r m, MonadIO m)

data ServerState = ServerState {
    serverSessions :: Map D.SessionId SessionData
  , serverInactiveSessions :: Map D.SessionId SessionData 
  , serverKnowUsers :: Set D.UserId
  , serverLobby :: Map GameType (Set D.SessionId)
  , serverActiveGames :: Map D.GameRoomId D.GameRoom
  , serverUserIdCounter :: D.UserIdx
  , serverSessionIdCounter :: D.SessionIdx -- common for all UserTypes
  , serverGameRoomIdCounter :: D.GameRoomIdx
  , serverWSToSend :: [D.SessionId]
  -- , serverWSChans :: Map D.SessionId WS.WSChan
  , serverGameStates :: Map D.SessionId (TVar G.GameState)
} 

data SessionData = SessionData 
  { sessionDataUserId :: D.UserId
  , sessioinDataWSChan :: Maybe WS.WSChan
  }

defaultSessionData :: D.UserId -> SessionData 
defaultSessionData userId = SessionData 
  { sessionDataUserId = userId
  , sessioinDataWSChan = Nothing
  }

initialServerState :: ServerState
initialServerState = ServerState {
    serverSessions = mempty
  , serverInactiveSessions = mempty
  , serverKnowUsers = mempty
  , serverLobby = mempty
  , serverActiveGames = mempty
  , serverUserIdCounter = 0
  , serverSessionIdCounter = 0
  , serverGameRoomIdCounter = 0
  , serverWSToSend = mempty
  , serverGameStates = mempty
}

initNewGuestSession :: InMemory r m => m (D.SessionId, D.UserId)
initNewGuestSession = do
  tvar :: TVar ServerState <- asks getter
  liftIO $ atomically $ do
    ss <- readTVar tvar
    let uId = serverUserIdCounter ss + 1
    let userId = D.UserId uId
    let sId = serverSessionIdCounter ss + 1
    let sessionId = D.SessionId sId
    let newSessionData = defaultSessionData userId
    let newSessions = Map.insert sessionId newSessionData (serverSessions ss)
    let newServerState = ss 
          { serverUserIdCounter = uId 
          , serverSessionIdCounter = sId
          , serverSessions = newSessions
          }
    writeTVar tvar newServerState
    pure (sessionId, userId)


initKnownUserSession :: InMemory r m => D.UserId -> m (Maybe (D.SessionId, D.UserId))
initKnownUserSession userId = do
  tvar :: TVar ServerState <- asks getter
  liftIO $ atomically $ do
    ss <- readTVar tvar
    if Set.notMember userId (serverKnowUsers ss)
      then pure Nothing
      else do
        let findSessionData = filter (\(_,sd) -> sessionDataUserId sd == userId ) $ Map.toList (serverInactiveSessions ss) 
        case findSessionData of
          [(sessionId,sd)] -> do
            let newSessionData = sd{sessioinDataWSChan = Nothing}
            let newSessions = Map.insert sessionId newSessionData (serverSessions ss)
            let newInactiveSessions = Map.delete sessionId (serverInactiveSessions ss)
            let newServerState = ss
                  { serverSessions = newSessions
                  , serverInactiveSessions = newInactiveSessions }
            writeTVar tvar newServerState
            pure $ Just (sessionId, userId)
          _ -> pure Nothing


restoreExistingSession :: InMemory r m => D.SessionId -> D.UserId -> m (Maybe (D.SessionId, D.UserId))
restoreExistingSession sessionId userId = do
  tvar :: TVar ServerState <- asks getter
  liftIO $ atomically $ do
    ss <- readTVar tvar
    case Map.lookup sessionId (serverInactiveSessions ss) of
      Nothing -> pure Nothing
      Just sd -> 
        if sessionDataUserId sd /= userId
          then pure Nothing
          else do
            let newSessionData = sd{sessioinDataWSChan = Nothing}
            let newSessions = Map.insert sessionId newSessionData (serverSessions ss)
            let newInactiveSessions = Map.delete sessionId (serverInactiveSessions ss)
            let newServerState = ss
                  { serverSessions = newSessions
                  , serverInactiveSessions = newInactiveSessions }
            writeTVar tvar newServerState
            pure $ Just (sessionId, userId)


disconnectSession :: InMemory r m => D.SessionId -> m ()
disconnectSession sessionId = do
  tvar :: TVar ServerState <- asks getter
  liftIO $ atomically $ do
    ss <- readTVar tvar
    case Map.lookup sessionId (serverSessions ss) of
      Nothing -> pure ()
      Just sd -> do
            let newSessionData = sd{sessioinDataWSChan = Nothing}
            let newInactiveSessions = Map.insert sessionId newSessionData (serverInactiveSessions ss)
            let newSessions = Map.delete sessionId (serverSessions ss)
            let newServerGameStates = Map.delete sessionId (serverGameStates ss)
            let newServerState = ss
                  { serverSessions = newSessions
                  , serverInactiveSessions = newInactiveSessions
                  , serverGameStates = newServerGameStates }
            writeTVar tvar newServerState

getUserIdBySessionId :: InMemory r m => D.SessionId -> m (Maybe D.UserId)
getUserIdBySessionId sessionId = do
  tvar :: TVar ServerState <- asks getter
  ss <- liftIO $ readTVarIO tvar
  case Map.lookup sessionId (serverSessions ss) of
    Nothing -> pure Nothing
    Just sd -> pure $ Just (sessionDataUserId sd)


initWSConn :: InMemory r m =>  WS.WSConnection -> D.SessionId -> m (Either Text ())
initWSConn wsConn sessionId = do
  tvar :: TVar ServerState <- asks getter
  liftIO $ atomically $ do
    ss <- readTVar tvar
    case Map.lookup sessionId (serverSessions ss) of
      Nothing -> pure $ Left "SessionId is not active"
      Just sd ->
        case sessioinDataWSChan sd of
          Just wsChan -> do
            let newWSChan = wsChan {wschanConn = wsConn}
            let newSessionData = sd{sessioinDataWSChan = Just newWSChan}
            let newSessions = Map.insert sessionId newSessionData (serverSessions ss)
            let newServerState = ss{ serverSessions = newSessions }
            writeTVar tvar newServerState
            pure $ Right ()
          Nothing -> do
            let newSessionData = sd{sessioinDataWSChan = Just $ WS.emptyWSChan wsConn}
            let newSessions = Map.insert sessionId newSessionData (serverSessions ss)
            let newServerState = ss{ serverSessions = newSessions }
            writeTVar tvar newServerState
            pure $ Right ()

disconnectWSConn :: InMemory r m => WS.WSConnection -> D.SessionId -> m ()
disconnectWSConn wsConn sessionId = do
  tvar :: TVar ServerState <- asks getter
  liftIO $ atomically $ do
    ss <- readTVar tvar
    case Map.lookup sessionId (serverSessions ss) of
      Nothing -> pure ()
      Just sd ->
        case sessioinDataWSChan sd of
          Just wsChan -> do
            let newSessionData = sd{sessioinDataWSChan = Nothing}
            let newSessions = Map.insert sessionId newSessionData (serverSessions ss)
            let newServerState = ss{ serverSessions = newSessions }
            writeTVar tvar newServerState
            pure ()
          _ -> pure ()


sendOutWSMessage :: InMemory r m => D.SessionId -> m ()
sendOutWSMessage sessionId = do
  tvar :: TVar ServerState <- asks getter
  mayMsg <- liftIO $ atomically $ do
    ss <- readTVar tvar
    case Map.lookup sessionId (serverSessions ss) of
      Nothing -> pure Nothing
      Just sd -> do
        case sessioinDataWSChan sd of
          Nothing -> pure Nothing
          Just chan@WSChan{wschanOut, wschanConn} -> do
            case wschanOut of
              [] -> pure Nothing
              msg:rest -> do
                let newWsChan = chan{wschanOut = rest}
                let newSessionData = sd{sessioinDataWSChan = Just newWsChan}
                let newSessions = Map.insert sessionId newSessionData (serverSessions ss)
                let newServerState = ss{ serverSessions = newSessions }
                writeTVar tvar newServerState
                pure $ Just (msg, wschanConn)
  case mayMsg of
    Nothing -> pure ()
    Just (msg, conn) -> liftIO $ WS.sendTextData conn msg 


pushInputWSMessage :: InMemory r m => D.SessionId -> WSMessage -> m ()
pushInputWSMessage sessionId msg = do
  tvar <- asks getter 
  liftIO $ atomically $ do
    ss <- readTVar tvar
    case Map.lookup sessionId (serverSessions ss) of
      Nothing -> pure ()
      Just sd -> do
        case sessioinDataWSChan sd of
          Nothing -> pure ()
          Just wsChan -> do
            let newWsChan = WS.pushWSIn msg wsChan 
            let newSessionData = sd{sessioinDataWSChan = Just newWsChan}
            let newSessions = Map.insert sessionId newSessionData (serverSessions ss)
            let newServerState = ss{ serverSessions = newSessions }
            writeTVar tvar newServerState
        

processWSMessagesEcho :: InMemory r m => D.SessionId -> m ()
processWSMessagesEcho sessionId = do
  tvar <- asks getter
  liftIO $ atomically $ do
    ss <- readTVar tvar
    case Map.lookup sessionId (serverSessions ss) of
      Nothing -> pure ()
      Just sd -> do
        case sessioinDataWSChan sd of
          Nothing -> pure ()
          Just chan@WSChan{wschanIn, wschanOut} -> do
            let newWsChan = chan{wschanIn = [], wschanOut = reverse wschanIn <> wschanOut }
            let newSessionData = sd{sessioinDataWSChan = Just newWsChan}
            let newSessions = Map.insert sessionId newSessionData (serverSessions ss)
            let newServerState = ss
                  { serverSessions = newSessions
                  , serverWSToSend = sessionId : serverWSToSend ss }
            writeTVar tvar newServerState


processWSMessages :: InMemory r m => ([WSMessage] -> WSMessage -> State G.GameState [WSMessage]) -> D.SessionId -> m ()
processWSMessages processWSMsg sessionId = do 
  tvar <- asks getter
  (msgs, tvarStates) <- liftIO $ atomically $ do
    ss <- readTVar tvar
    let gameStates = serverGameStates ss
    case Map.lookup sessionId (serverSessions ss) of
      Nothing -> pure ([], gameStates)
      Just sd -> do
        case sessioinDataWSChan sd of
          Nothing -> pure ([], gameStates)
          Just chan@WSChan{wschanIn} -> do
            let newWsChan = chan{wschanIn = []}
            let newSessionData = sd{sessioinDataWSChan = Just newWsChan}
            let newSessions = Map.insert sessionId newSessionData (serverSessions ss)
            let newServerState = ss{ serverSessions = newSessions }
            writeTVar tvar newServerState
            pure (wschanIn, gameStates) 

  unless (null msgs) $ do
    case Map.lookup sessionId tvarStates of
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
          ss <- readTVar tvar
          case Map.lookup sessionId (serverSessions ss) of
            Nothing -> pure ()
            Just sd -> do
              case sessioinDataWSChan sd of
                Nothing -> pure ()
                Just chan@WSChan{wschanOut} -> do
                  let newWsChan = chan{wschanOut = reverse outMsgs <> wschanOut}
                  let newSessionData = sd{sessioinDataWSChan = Just newWsChan}
                  let newSessions = Map.insert sessionId newSessionData (serverSessions ss)
                  let newServerState = ss
                        { serverSessions = newSessions
                        , serverWSToSend = sessionId : serverWSToSend ss }
                  writeTVar tvar newServerState


sendOutAllWSMessages :: InMemory r m => m ()
sendOutAllWSMessages = do
  tvar <- asks getter
  sIds <- liftIO $ atomically $ do
    ss <- readTVar tvar
    let ids = serverWSToSend ss
    writeTVar tvar ss{serverWSToSend = []}
    pure ids
  traverse_ sendOutWSMessage sIds


-- -- TODO check there is no active game for this sId
-- addGameToLobby :: InMemory r m => D.SessionId -> GameType -> m ()
-- addGameToLobby sId gameType = do
--   tvar <- asks getter
--   liftIO $ atomically $ do
--     ss :: ServerState <- readTVar tvar
--     let newLobby = Map.update (Just . Set.insert sId) gameType (serverLobby ss)
--     writeTVar tvar ss{serverLobby = newLobby}


-- startGame :: InMemory r m => D.SessionIdHost -> D.SessionIdGuest -> GameType -> m D.GameRoomId
-- startGame sHostId sGuestId gameType = do
--   tvar <- asks getter
--   let newGameRoom = D.GameRoom
--         { D.gameRoomGameType = gameType
--         , D.gameRoomHost = sHostId
--         , D.gameRoomGuest = sGuestId
--         }
--   liftIO $ atomically $ do
--     ss :: ServerState <- readTVar tvar
--     let grIdx = serverGameRoomIdCounter ss + 1
--     let gameRoomId = D.GameRoomId grIdx
--     let newLobby = Map.update (Just . Set.delete sHostId ) gameType (serverLobby ss)
--     let newActiveGames = Map.insert gameRoomId newGameRoom (serverActiveGames ss)
--     writeTVar tvar ss{serverLobby = newLobby, serverActiveGames = newActiveGames, serverGameRoomIdCounter = grIdx}
--     pure gameRoomId
