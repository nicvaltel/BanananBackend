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
  , addGameToLobby
  , joinGame
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

data LobbyElem = LobbyElem {lobbyLobbyId :: D.LobbyId, lobbySessionIdHost :: D.SessionId, lobbyGameType :: GameType}
  deriving (Show, Eq, Ord)



data ServerState = ServerState {
    serverSessions :: Map D.SessionId SessionData
  , serverInactiveSessions :: Map D.SessionId SessionData 
  , serverKnowUsers :: Set D.UserId
  , serverLobby :: [LobbyElem]
  , serverActiveGames :: Map D.GameRoomId D.GameRoom
  , serverUserIdCounter :: D.UserId
  , serverSessionIdCounter :: D.SessionId -- common for all UserTypes
  , serverGameRoomIdCounter :: D.GameRoomId
  , serverLobbyIdCounter :: D.LobbyId
  , serverWSToSend :: [D.SessionId]
  -- , serverWSChans :: Map D.SessionId WS.WSChan
  , serverGameStates :: Map D.SessionId (TVar G.GameState)
} 

data SessionData = SessionData 
  { sessionDataUserId :: D.UserId
  , sessioinDataWSChan :: Maybe (TVar WS.WSChan)
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
  , serverLobbyIdCounter = 0
  , serverWSToSend = mempty
  , serverGameStates = mempty
}

initNewGuestSession :: InMemory r m => m (D.SessionId, D.UserId)
initNewGuestSession = do
  tvar :: TVar ServerState <- asks getter
  liftIO $ atomically $ do
    ss <- readTVar tvar
    let userId = serverUserIdCounter ss + 1
    let sessionId = serverSessionIdCounter ss + 1
    let newSessionData = defaultSessionData userId
    let newSessions = Map.insert sessionId newSessionData (serverSessions ss)
    let newServerState = ss 
          { serverUserIdCounter = userId 
          , serverSessionIdCounter = sessionId
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


initWSConn :: InMemory r m =>  WS.WSConnection -> D.SessionId -> m (Either D.SessionError ())
initWSConn wsConn sessionId = do
  tvar :: TVar ServerState <- asks getter
  liftIO $ atomically $ do
    ss <- readTVar tvar
    case Map.lookup sessionId (serverSessions ss) of
      Nothing -> pure $ Left D.SessionErrorSessionIdIsNotActive
      Just sd ->
        case sessioinDataWSChan sd of
          Just tvarWsChan -> do
            wsChan <- readTVar tvarWsChan
            let newWSChan = wsChan {wschanConn = wsConn}
            writeTVar tvarWsChan newWSChan
            pure $ Right ()
          Nothing -> do
            tvarWsChan <- newTVar (WS.emptyWSChan wsConn)
            let newSessionData = sd{sessioinDataWSChan = Just tvarWsChan}
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
          Just tvarWsChan -> do
            let newSessionData = sd{sessioinDataWSChan = Nothing}
            let newSessions = Map.insert sessionId newSessionData (serverSessions ss)
            let newServerState = ss{ serverSessions = newSessions }
            writeTVar tvar newServerState
            pure ()
          _ -> pure ()


sendOutWSMessage :: InMemory r m => D.SessionId -> m ()
sendOutWSMessage sessionId = do
  tvar :: TVar ServerState <- asks getter
  ss <- readTVarIO tvar
  case Map.lookup sessionId (serverSessions ss) of
      Nothing -> pure ()
      Just sd ->
        case sessioinDataWSChan sd of
          Nothing -> pure ()
          Just tvarWsChan -> do
            chan@WSChan{wschanOut, wschanConn} <- readTVarIO tvarWsChan
            case wschanOut of
              [] -> pure ()
              msg:rest -> do
                let newWsChan = chan{wschanOut = rest}
                atomically (writeTVar tvarWsChan newWsChan)
                liftIO $ WS.sendTextData wschanConn msg 


pushInputWSMessage :: InMemory r m => D.SessionId -> WSMessage -> m ()
pushInputWSMessage sessionId msg = do
  tvar <- asks getter 
  ss <- readTVarIO tvar
  case Map.lookup sessionId (serverSessions ss) of
      Nothing -> pure ()
      Just sd ->
        case sessioinDataWSChan sd of
          Nothing -> pure ()
          Just tvarWsChan -> do
            chan <- readTVarIO tvarWsChan
            let newWsChan = WS.pushWSIn msg chan
            atomically (writeTVar tvarWsChan newWsChan)
        

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
          Just tvarWsChan -> do
            chan@WSChan{wschanOut, wschanIn} <- readTVar tvarWsChan
            let newWsChan = chan{wschanIn = [], wschanOut = reverse wschanIn <> wschanOut }
            writeTVar tvarWsChan newWsChan
            let newServerState = ss { serverWSToSend = sessionId : serverWSToSend ss } -- TODO do it without updating all server
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
          Just tvarWsChan -> do
            chan@WSChan{wschanIn} <- readTVar tvarWsChan
            let newWsChan = chan{wschanIn = []}
            writeTVar tvarWsChan newWsChan
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
                Just tvarWsChan -> do
                  chan@WSChan{wschanOut} <- readTVar tvarWsChan
                  let newWsChan = chan{wschanOut = reverse outMsgs <> wschanOut}
                  writeTVar tvarWsChan newWsChan
                  let newServerState = ss { serverWSToSend = sessionId : serverWSToSend ss } -- TODO do it without updating all server
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


addGameToLobby :: InMemory r m => D.SessionId -> GameType -> m (Either D.LobbyError D.LobbyId)
addGameToLobby sessionIdHost lobbyGameType = do
  tvar <- asks getter
  liftIO $ atomically $ do
    ss :: ServerState <- readTVar tvar
    if Map.member sessionIdHost (serverGameStates ss)
      then pure $ Left D.LobbyErrorActiveGameIsGoingOn
      else if any (\lb -> lobbySessionIdHost lb == sessionIdHost) (serverLobby ss)
        then pure $ Left D.LobbyErrorGameOrderIsInTheLobby
        else do
          let lobbyLobbyId = serverLobbyIdCounter ss + 1
          let newServerLobby = LobbyElem{lobbyLobbyId, lobbySessionIdHost = sessionIdHost, lobbyGameType} : serverLobby ss
          writeTVar tvar ss{
                serverLobby = newServerLobby
              , serverLobbyIdCounter = lobbyLobbyId}
          pure $ Right lobbyLobbyId



joinGame :: InMemory r m => D.SessionIdGuest -> D.LobbyId -> m (Maybe D.GameRoomId)
joinGame sIdGuest lobbyId = do
  tvar <- asks getter
  
  liftIO $ atomically $ do
    ss :: ServerState <- readTVar tvar
    case partition (\lb -> lobbyLobbyId lb == lobbyId) (serverLobby ss) of
        ([LobbyElem{lobbySessionIdHost, lobbyGameType}],newLobby) -> do 
            tvarGameStHost <- newTVar G.initialGameState
            tvarGameStGuest <- newTVar G.initialGameState

            let newGameRoom = D.GameRoom
                  { D.gameRoomGameType = lobbyGameType
                  , D.gameRoomHost = lobbySessionIdHost
                  , D.gameRoomGuest = sIdGuest
                  }
            let gameRoomId = serverGameRoomIdCounter ss + 1
            let newActiveGames = Map.insert gameRoomId newGameRoom (serverActiveGames ss)
            let newServerGameStates = Map.insert lobbySessionIdHost tvarGameStHost $
                                      Map.insert sIdGuest tvarGameStGuest $ 
                                      serverGameStates ss
            writeTVar tvar ss{
                    serverLobby = newLobby, 
                    serverActiveGames = newActiveGames, 
                    serverGameRoomIdCounter = gameRoomId,
                    serverGameStates = newServerGameStates}
            pure (Just $ D.GameRoomId 0)
        _ -> pure Nothing 