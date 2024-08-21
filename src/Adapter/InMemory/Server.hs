{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
module Adapter.InMemory.Server where

import ClassyPrelude
import qualified Domain.Server as D
import Domain.Game (GameType)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified WebSocketServer as WS 
import WebSocketServer(WSMessage, WSChan(..), WSConnection)
import qualified Domain.GameBot.GameModel as G
import Domain.Server (LobbyEntry(..))
import Control.Lens
import Data.Has (Has(..))
import Text.StringRandom (stringRandomIO)


type InMemory r m = (Has (TVar ServerState) r, MonadReader r m, MonadIO m)

data SessionData = SessionData
  { _sdMayUserId :: Maybe D.UserId -- Nothing for guest
  , _sdWSConn :: WSConnection
  , _sdWSChan :: WS.WSChan
  }

data ServerState = ServerState 
  { _serverSessions :: Map D.SessionId SessionData
  , _serverUserIdCounter :: D.UserId
  , _serverLobby :: [LobbyEntry]
  , _serverLobbyIdCounter :: D.LobbyId
  , _serverActiveGames :: Map D.GameRoomId D.GameRoom
  , _serverGameRoomIdCounter :: D.GameRoomId
  , _serverSessionsWithActiveGames :: Set D.SessionId
  , _serverWSToSend :: [D.SessionId]
  } 

makeLenses ''SessionData
makeLenses ''ServerState


initialServerState :: ServerState
initialServerState = ServerState {
    _serverSessions = mempty
  , _serverLobby = mempty
  , _serverActiveGames = mempty
  , _serverUserIdCounter = 0
  , _serverGameRoomIdCounter = 0
  , _serverLobbyIdCounter = 0
  , _serverWSToSend = mempty
  , _serverSessionsWithActiveGames = mempty
}


newSession :: InMemory r m => Maybe D.UserId -> D.SessionId -> WSConnection -> m (Either D.SessionError WS.WSChan)
newSession mayUid sId conn = do
  chan <- atomically $ WS.emptyWSChan conn
  let sessionData = SessionData
        { _sdMayUserId = mayUid
        , _sdWSConn = conn
        , _sdWSChan = chan
        }
  tvar :: TVar ServerState <- asks getter
  liftIO $ atomically $ do
    state <- readTVar tvar
    writeTVar tvar $
      state & serverSessions %~ Map.insert sId sessionData
  pure $ Right chan


findUserBySessionId :: InMemory r m => D.SessionId -> m (Maybe D.UserId)
findUserBySessionId sId = do
  tvar :: TVar ServerState <- asks getter
  state <- readTVarIO tvar
  pure $ _sdMayUserId =<< Map.lookup sId (state ^. serverSessions)


disconnectWSConn :: InMemory r m => WS.WSConnection -> D.SessionId -> m ()
disconnectWSConn wsConn sId = do
  tvar :: TVar ServerState <- asks getter
  atomically $ do
    state <- readTVar tvar
    case Map.lookup sId (state ^. serverSessions) of
      Nothing -> pure ()
      Just _ -> do
        writeTVar tvar $
          state & serverSessions %~ Map.delete sId


-- sendOutWSMessage :: InMemory r m => D.SessionId -> m ()
-- sendOutWSMessage sessionId = do
--   tvar :: TVar ServerState <- asks getter
--   ss <- readTVarIO tvar
--   case Map.lookup sessionId (serverSessions ss) of
--       Nothing -> pure ()
--       Just sd ->
--         case D.sessioinDataWSChan sd of
--           Nothing -> pure ()
--           Just tvarWsChan -> do
--             chan@WSChan{wschanOut, wschanConn} <- readTVarIO tvarWsChan
--             case wschanOut of
--               [] -> pure ()
--               msg:rest -> do
--                 let newWsChan = chan{wschanOut = rest}
--                 atomically (writeTVar tvarWsChan newWsChan)
--                 liftIO $ WS.sendTextData wschanConn msg 

-- getWSChanBySessionId :: InMemory r m => D.SessionId -> m (Maybe WSChan)
-- getWSChanBySessionId sId = do
--   tvar :: TVar ServerState <- asks getter
--   ss <- readTVarIO tvar
--   case Map.lookup sId (serverSessions ss) of
--       Nothing -> pure Nothing
--       Just sd ->
--         case D.sessioinDataWSChan sd of
--           Nothing -> pure Nothing
--           Just tvarWsChan -> do
--             chan <- readTVarIO tvarWsChan
--             pure $ Just chan


-- pushInputWSMessage :: InMemory r m => D.SessionId -> D.GameRoomId -> WSMessage -> m ()
-- pushInputWSMessage sId gId msg = do
--   tvar <- asks getter
--   atomically $ do
--     state <- readTVar tvar
--     case Map.lookup gId (state ^. serverActiveGames) of
--       Nothing -> pure ()
--       Just room@D.GameRoom{D.gameRoomHost = grHost, D.gameRoomGuest = grGuest} -> do
--         when (fst grHost == sId) $
--           writeTVar tvar $
--             state & serverActiveGames %~ Map.insert gId room{D.gameRoomHost = (sId, Nothing)}
--         when (fst grGuest == sId) $
--           writeTVar tvar $
--             state & serverActiveGames %~ Map.insert gId room{D.gameRoomGuest = (sId, Nothing)}


      

-- processWSMessagesEcho :: InMemory r m => D.SessionId -> m ()
-- processWSMessagesEcho sessionId = do
--   tvar <- asks getter
--   liftIO $ atomically $ do
--     ss <- readTVar tvar
--     case Map.lookup sessionId (serverSessions ss) of
--       Nothing -> pure ()
--       Just sd -> do
--         case D.sessioinDataWSChan sd of
--           Nothing -> pure ()
--           Just tvarWsChan -> do
--             chan@WSChan{wschanOut, wschanIn} <- readTVar tvarWsChan
--             let newWsChan = chan{wschanIn = [], wschanOut = reverse wschanIn <> wschanOut }
--             writeTVar tvarWsChan newWsChan
--             let newServerState = ss { serverWSToSend = sessionId : serverWSToSend ss } -- TODO do it without updating all server
--             writeTVar tvar newServerState


-- processWSMessages :: InMemory r m => (D.SessionId -> WSMessage -> m (Maybe (WSConnection, WSMessage))) -> D.SessionId -> m ()
-- processWSMessages processWSMsg sId = do 
--   tvar <- asks getter
--   ss <- readTVarIO tvar
--   case Map.lookup sId (serverSessions ss) of
--     Nothing -> pure ()
--     Just sd -> do
--       case D.sessioinDataWSChan sd of
--         Nothing -> pure ()
--         Just tvarWsChan -> do
--           inMsgs <- liftIO $ atomically $ do
--             chan@WSChan{wschanIn} <- readTVar tvarWsChan
--             let newWsChan = chan{wschanIn = []}
--             writeTVar tvarWsChan newWsChan
--             pure wschanIn
--           mayOutConnMsgs <- traverse (processWSMsg sId) inMsgs
--           liftIO $ traverse_ (uncurry WS.sendTextData) (catMaybes mayOutConnMsgs) 


-- processWSMessages' :: InMemory r m => ([WSMessage] -> WSMessage -> State G.GameState [WSMessage]) -> D.SessionId -> m ()
-- processWSMessages' processWSMsg sessionId = do 
--   tvar <- asks getter
--   ss <- readTVarIO tvar
--   let gameStates = serverGameStates ss
--   msgs <- case Map.lookup sessionId (serverSessions ss) of
--       Nothing -> pure []
--       Just sd -> do
--         case D.sessioinDataWSChan sd of
--           Nothing -> pure []
--           Just tvarWsChan -> liftIO $ atomically $ do
--             chan@WSChan{wschanIn} <- readTVar tvarWsChan
--             let newWsChan = chan{wschanIn = []}
--             writeTVar tvarWsChan newWsChan
--             pure wschanIn
--   unless (null msgs) $ do
--     case Map.lookup sessionId (serverGameStates ss) of
--       Nothing -> pure () -- TODO here is the problem!
--       Just tvSt -> do
--         outMsgs <- liftIO $ atomically $ do
--           gs <- readTVar tvSt
--           -- foldM :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b
--           -- m :: State GameState
--           -- m b :: State GameState [WSMessage]
--           -- a :: WSMessage
--           -- b :: [WSMessage]
--           let (outMsgs, newGs) = runState (foldM processWSMsg [] (reverse msgs)) gs
--           writeTVar tvSt  newGs
--           pure outMsgs

--         unless (null outMsgs) $ liftIO $ atomically $ do
--           ss <- readTVar tvar
--           case Map.lookup sessionId (serverSessions ss) of
--             Nothing -> pure ()
--             Just sd -> do
--               case D.sessioinDataWSChan sd of
--                 Nothing -> pure ()
--                 Just tvarWsChan -> do
--                   chan@WSChan{wschanOut} <- readTVar tvarWsChan
--                   let newWsChan = chan{wschanOut = reverse outMsgs <> wschanOut}
--                   writeTVar tvarWsChan newWsChan
--                   let newServerState = ss { serverWSToSend = sessionId : serverWSToSend ss } -- TODO do it without updating all server
--                   writeTVar tvar newServerState


-- sendOutAllWSMessages :: InMemory r m => m ()
-- sendOutAllWSMessages = do
--   tvar <- asks getter
--   sIds <- liftIO $ atomically $ do
--     ss <- readTVar tvar
--     let ids = serverWSToSend ss
--     writeTVar tvar ss{serverWSToSend = []}
--     pure ids
--   traverse_ sendOutWSMessage sIds


addGameToLobby :: InMemory r m => D.SessionId -> WSConnection -> GameType -> m (Either D.LobbyError D.LobbyId)
addGameToLobby sessionIdHost lobbyWSConnectionHost lobbyGameType = do
  tvar <- asks getter
  liftIO $ atomically $ do
    state :: ServerState <- readTVar tvar
    if Set.member sessionIdHost (state ^. serverSessionsWithActiveGames)
      then pure $ Left D.LobbyErrorActiveGameIsGoingOn
      else if any (\lb -> lobbySessionIdHost lb == sessionIdHost) (state ^. serverLobby)
        then pure $ Left D.LobbyErrorGameOrderIsInTheLobby
        else do
          let lobbyLobbyId = state ^. serverLobbyIdCounter
          let newLobby = LobbyEntry
                { lobbyLobbyId
                , lobbySessionIdHost = sessionIdHost
                , lobbyWSConnectionHost
                , lobbyGameType
                , lobbyMaybeGameRoomId = Nothing
                }
          writeTVar tvar $
            state & serverLobbyIdCounter .~ lobbyLobbyId
                  & serverLobby %~ (newLobby :)
          pure $ Right lobbyLobbyId

getLobbyEntries :: InMemory r m => m [LobbyEntry]
getLobbyEntries = asks getter >>= (_serverLobby <$>) . readTVarIO 


joinGame :: InMemory r m => D.SessionIdGuest -> WSConnection -> D.LobbyId -> m (Maybe D.GameRoomId)
joinGame sIdGuest wsConnGuest lobbyId = do
  let wsChanGuest = WS.emptyWSChan wsConnGuest
  tvar <- asks getter
  liftIO $ atomically $ do
    state :: ServerState <- readTVar tvar
    case partition (\lb -> lobbyLobbyId lb == lobbyId) (state ^. serverLobby) of
        ([lbEntry@LobbyEntry{lobbySessionIdHost, lobbyGameType, lobbyWSConnectionHost}],restLobby) -> do 
            tvarGameState <- newTVar G.initialGameState
            hostChan <- WS.emptyWSChan lobbyWSConnectionHost
            guestChan <- WS.emptyWSChan wsConnGuest
            let wsChanHost = WS.emptyWSChan lobbyWSConnectionHost
            let newGameRoom = D.GameRoom
                  { D.gameRoomGameType = lobbyGameType
                  , D.gameRoomHost = lobbySessionIdHost
                  , D.gameRoomGuest = sIdGuest
                  , D.gameRoomHostChan = hostChan
                  , D.gameRoomGuestChan = guestChan 
                  , D.gameRoomGameState = tvarGameState
                  }
            let gameRoomId = state ^. serverGameRoomIdCounter + 1
            let newLobbys = lbEntry{lobbyMaybeGameRoomId = Just gameRoomId} : restLobby
            writeTVar tvar $
              state & serverGameRoomIdCounter .~ gameRoomId
                    & serverLobby .~ newLobbys
                    & serverActiveGames %~ Map.insert gameRoomId newGameRoom
                    & serverSessionsWithActiveGames %~ (Set.insert sIdGuest . Set.insert lobbySessionIdHost)
            pure (Just gameRoomId)
        _ -> pure Nothing 


-- startGameWithBot :: InMemory r m => D.SessionId -> GameType -> m (Either D.LobbyError D.GameRoomId)
-- startGameWithBot sessionIdHost gameType = do
--   tvar <- asks getter
--   liftIO $ atomically $ do
--     ss :: ServerState <- readTVar tvar
--     if Map.member sessionIdHost (serverGameStates ss)
--       then pure $ Left D.LobbyErrorActiveGameIsGoingOn
--       else if any (\lb -> lobbySessionIdHost lb == sessionIdHost) (serverLobby ss)
--         then pure $ Left D.LobbyErrorGameOrderIsInTheLobby
--         else do
--           tvarGameStHost <- newTVar G.initialGameState
--           tvarGameStBot <- newTVar G.initialGameState
--           let sessionIdBot = undefined
--           let newGameRoom = D.GameRoom
--                 { D.gameRoomGameType = gameType
--                 , D.gameRoomHost = sessionIdHost
--                 , D.gameRoomGuest = sessionIdBot
--                 }
--           let gameRoomId = serverGameRoomIdCounter ss + 1
--           let newActiveGames = Map.insert gameRoomId newGameRoom (serverActiveGames ss)
--           let newServerGameStates = Map.insert sessionIdHost tvarGameStHost $
--                                     Map.insert sessionIdBot tvarGameStBot $ 
--                                     serverGameStates ss
--           writeTVar tvar ss{
--                   serverActiveGames = newActiveGames, 
--                   serverGameRoomIdCounter = gameRoomId,
--                   serverGameStates = newServerGameStates}
--           pure $ Right gameRoomId


