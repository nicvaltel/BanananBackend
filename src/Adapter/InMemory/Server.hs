{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Adapter.InMemory.Server 
  ( ServerState
  , initialServerState
  , initNewGuestSession
  , initKnownUserSession
  , restoreExistingSession
  , disconnectSession
  , getSessionDataBySessionId
  , initWSConn
  , pushInputWSMessage
  , processWSMessagesEcho
  , processWSMessages
  , sendOutWSMessage
  , sendOutAllWSMessages
  , disconnectWSConn
  , addGameToLobby
  , joinGame
  , initBotSession
  , startGameWithBot
  , getWSChanBySessionId
  , getLobbyEntries
  ) where

import Reexport
import ClassyPrelude
import qualified Domain.Server as D
import Domain.Game (GameType)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified WebSocketServer as WS 
import WebSocketServer(WSMessage, WSChan(..), WSConnection)
import qualified Domain.GameBot.GameModel as G
import Domain.Server (LobbyEntry(..))

type InMemory r m = (Has (TVar ServerState) r, MonadReader r m, MonadIO m)




data ServerState = ServerState {
    serverSessions :: Map D.SessionId D.SessionData
  , serverInactiveSessions :: Map D.SessionId D.SessionData 
  , serverKnowUsers :: Set D.UserId
  , serverLobby :: [LobbyEntry]
  , serverActiveGames :: Map D.GameRoomId D.GameRoom
  , serverUserIdCounter :: D.UserId
  , serverSessionIdCounter :: D.SessionId -- common for all UserTypes
  , serverGameRoomIdCounter :: D.GameRoomId
  , serverLobbyIdCounter :: D.LobbyId
  , serverWSToSend :: [D.SessionId]
  -- , serverWSChans :: Map D.SessionId WS.WSChan
  , serverGameStates :: Map D.SessionId (TVar G.GameState)
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

initNewGuestSession :: InMemory r m => m (D.SessionId, D.UserId, D.Token)
initNewGuestSession = do
  tvar :: TVar ServerState <- asks getter
  token <- liftIO $ stringRandomIO "[A-Za-z0-9]{16}"
  liftIO $ atomically $ do
    ss <- readTVar tvar
    let userId = serverUserIdCounter ss + 1
    let sessionId = serverSessionIdCounter ss + 1
    let newSessionData = D.defaultSessionData userId token
    let newSessions = Map.insert sessionId newSessionData (serverSessions ss)
    let newServerState = ss 
          { serverUserIdCounter = userId 
          , serverSessionIdCounter = sessionId
          , serverSessions = newSessions
          }
    writeTVar tvar newServerState
    pure (sessionId, userId, token)


initKnownUserSession :: InMemory r m => D.UserId -> m (Maybe (D.SessionId, D.UserId))
initKnownUserSession userId = do
  tvar :: TVar ServerState <- asks getter
  liftIO $ atomically $ do
    ss <- readTVar tvar
    if Set.notMember userId (serverKnowUsers ss)
      then pure Nothing
      else do
        let findSessionData = filter (\(_,sd) -> D.sessionDataUserId sd == userId ) $ Map.toList (serverInactiveSessions ss) 
        case findSessionData of
          [(sessionId,sd)] -> do
            let newSessionData = sd{D.sessioinDataWSChan = Nothing}
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
        if D.sessionDataUserId sd /= userId
          then pure Nothing
          else do
            let newSessionData = sd{D.sessioinDataWSChan = Nothing}
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
            let newSessionData = sd{D.sessioinDataWSChan = Nothing}
            let newInactiveSessions = Map.insert sessionId newSessionData (serverInactiveSessions ss)
            let newSessions = Map.delete sessionId (serverSessions ss)
            let newServerGameStates = Map.delete sessionId (serverGameStates ss)
            let newServerState = ss
                  { serverSessions = newSessions
                  , serverInactiveSessions = newInactiveSessions
                  , serverGameStates = newServerGameStates }
            writeTVar tvar newServerState

initBotSession :: InMemory r m => m (D.SessionId, D.UserId)
initBotSession = do
  tvar :: TVar ServerState <- asks getter
  liftIO $ atomically $ do
    ss <- readTVar tvar
    let userId = serverUserIdCounter ss + 1
    let sessionId = serverSessionIdCounter ss + 1
    let newSessionData = D.defaultSessionData userId ""
    let newSessions = Map.insert sessionId newSessionData (serverSessions ss)
    let newServerState = ss 
          { serverUserIdCounter = userId 
          , serverSessionIdCounter = sessionId
          , serverSessions = newSessions
          }
    writeTVar tvar newServerState
    pure (sessionId, userId)

getSessionDataBySessionId :: InMemory r m => D.SessionId -> m (Maybe D.SessionData)
getSessionDataBySessionId sessionId = do
  tvar :: TVar ServerState <- asks getter
  ss <- liftIO $ readTVarIO tvar
  pure $ Map.lookup sessionId (serverSessions ss)


initWSConn :: InMemory r m =>  WS.WSConnection -> D.SessionId -> m (Either D.SessionError ())
initWSConn wsConn sessionId = do
  tvar :: TVar ServerState <- asks getter
  liftIO $ atomically $ do
    ss <- readTVar tvar
    case Map.lookup sessionId (serverSessions ss) of
      Nothing -> pure $ Left D.SessionErrorSessionIdIsNotActive
      Just sd ->
        case D.sessioinDataWSChan sd of
          Just tvarWsChan -> do
            wsChan <- readTVar tvarWsChan
            let newWSChan = wsChan {wschanConn = wsConn}
            writeTVar tvarWsChan newWSChan
            pure $ Right ()
          Nothing -> do
            tvarWsChan <- newTVar (WS.emptyWSChan wsConn)
            let newSessionData = sd{D.sessioinDataWSChan = Just tvarWsChan}
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
        case D.sessioinDataWSChan sd of
          Just tvarWsChan -> do
            let newSessionData = sd{D.sessioinDataWSChan = Nothing}
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
        case D.sessioinDataWSChan sd of
          Nothing -> pure ()
          Just tvarWsChan -> do
            chan@WSChan{wschanOut, wschanConn} <- readTVarIO tvarWsChan
            case wschanOut of
              [] -> pure ()
              msg:rest -> do
                let newWsChan = chan{wschanOut = rest}
                atomically (writeTVar tvarWsChan newWsChan)
                liftIO $ WS.sendTextData wschanConn msg 

getWSChanBySessionId :: InMemory r m => D.SessionId -> m (Maybe WSChan)
getWSChanBySessionId sId = do
  tvar :: TVar ServerState <- asks getter
  ss <- readTVarIO tvar
  case Map.lookup sId (serverSessions ss) of
      Nothing -> pure Nothing
      Just sd ->
        case D.sessioinDataWSChan sd of
          Nothing -> pure Nothing
          Just tvarWsChan -> do
            chan <- readTVarIO tvarWsChan
            pure $ Just chan


pushInputWSMessage :: InMemory r m => D.SessionId -> WSMessage -> m ()
pushInputWSMessage sessionId msg = do
  tvar <- asks getter 
  ss <- readTVarIO tvar
  case Map.lookup sessionId (serverSessions ss) of
      Nothing -> pure ()
      Just sd ->
        case D.sessioinDataWSChan sd of
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
        case D.sessioinDataWSChan sd of
          Nothing -> pure ()
          Just tvarWsChan -> do
            chan@WSChan{wschanOut, wschanIn} <- readTVar tvarWsChan
            let newWsChan = chan{wschanIn = [], wschanOut = reverse wschanIn <> wschanOut }
            writeTVar tvarWsChan newWsChan
            let newServerState = ss { serverWSToSend = sessionId : serverWSToSend ss } -- TODO do it without updating all server
            writeTVar tvar newServerState


processWSMessages :: InMemory r m => (D.SessionId -> WSMessage -> m (Maybe (WSConnection, WSMessage))) -> D.SessionId -> m ()
processWSMessages processWSMsg sId = do 
  tvar <- asks getter
  ss <- readTVarIO tvar
  case Map.lookup sId (serverSessions ss) of
    Nothing -> pure ()
    Just sd -> do
      case D.sessioinDataWSChan sd of
        Nothing -> pure ()
        Just tvarWsChan -> do
          inMsgs <- liftIO $ atomically $ do
            chan@WSChan{wschanIn} <- readTVar tvarWsChan
            let newWsChan = chan{wschanIn = []}
            writeTVar tvarWsChan newWsChan
            pure wschanIn
          mayOutConnMsgs <- traverse (processWSMsg sId) inMsgs
          liftIO $ traverse_ (uncurry WS.sendTextData) (catMaybes mayOutConnMsgs) 


processWSMessages' :: InMemory r m => ([WSMessage] -> WSMessage -> State G.GameState [WSMessage]) -> D.SessionId -> m ()
processWSMessages' processWSMsg sessionId = do 
  tvar <- asks getter
  ss <- readTVarIO tvar
  let gameStates = serverGameStates ss
  msgs <- case Map.lookup sessionId (serverSessions ss) of
      Nothing -> pure []
      Just sd -> do
        case D.sessioinDataWSChan sd of
          Nothing -> pure []
          Just tvarWsChan -> liftIO $ atomically $ do
            chan@WSChan{wschanIn} <- readTVar tvarWsChan
            let newWsChan = chan{wschanIn = []}
            writeTVar tvarWsChan newWsChan
            pure wschanIn
  unless (null msgs) $ do
    case Map.lookup sessionId (serverGameStates ss) of
      Nothing -> pure () -- TODO here is the problem!
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
              case D.sessioinDataWSChan sd of
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
          let newServerLobby = LobbyEntry{lobbyLobbyId, lobbySessionIdHost = sessionIdHost, lobbyGameType, lobbyMaybeGameRoomId = Nothing} : serverLobby ss
          writeTVar tvar ss{
                serverLobby = newServerLobby
              , serverLobbyIdCounter = lobbyLobbyId}
          pure $ Right lobbyLobbyId

getLobbyEntries :: InMemory r m => m [LobbyEntry]
getLobbyEntries = asks getter >>= (serverLobby <$>) . readTVarIO 


joinGame :: InMemory r m => D.SessionIdGuest -> D.LobbyId -> m (Maybe D.GameRoomId)
joinGame sIdGuest lobbyId = do
  tvar <- asks getter
  liftIO $ atomically $ do
    ss :: ServerState <- readTVar tvar
    case partition (\lb -> lobbyLobbyId lb == lobbyId) (serverLobby ss) of
        ([lbEntry@LobbyEntry{lobbySessionIdHost, lobbyGameType}],restLobby) -> do 
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
                    serverLobby = lbEntry{lobbyMaybeGameRoomId = Just gameRoomId} : restLobby, 
                    serverActiveGames = newActiveGames, 
                    serverGameRoomIdCounter = gameRoomId,
                    serverGameStates = newServerGameStates}
            pure (Just gameRoomId)
        _ -> pure Nothing 


startGameWithBot :: InMemory r m => D.SessionId -> GameType -> m (Either D.LobbyError D.GameRoomId)
startGameWithBot sessionIdHost gameType = do
  tvar <- asks getter
  liftIO $ atomically $ do
    ss :: ServerState <- readTVar tvar
    if Map.member sessionIdHost (serverGameStates ss)
      then pure $ Left D.LobbyErrorActiveGameIsGoingOn
      else if any (\lb -> lobbySessionIdHost lb == sessionIdHost) (serverLobby ss)
        then pure $ Left D.LobbyErrorGameOrderIsInTheLobby
        else do
          tvarGameStHost <- newTVar G.initialGameState
          tvarGameStBot <- newTVar G.initialGameState
          let sessionIdBot = undefined
          let newGameRoom = D.GameRoom
                { D.gameRoomGameType = gameType
                , D.gameRoomHost = sessionIdHost
                , D.gameRoomGuest = sessionIdBot
                }
          let gameRoomId = serverGameRoomIdCounter ss + 1
          let newActiveGames = Map.insert gameRoomId newGameRoom (serverActiveGames ss)
          let newServerGameStates = Map.insert sessionIdHost tvarGameStHost $
                                    Map.insert sessionIdBot tvarGameStBot $ 
                                    serverGameStates ss
          writeTVar tvar ss{
                  serverActiveGames = newActiveGames, 
                  serverGameRoomIdCounter = gameRoomId,
                  serverGameStates = newServerGameStates}
          pure $ Right gameRoomId


