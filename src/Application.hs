{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Avoid lambda" #-}
module Application where


import Reexport
import Lib
import WebSocketServer
import Control.Concurrent (threadDelay)
import qualified Domain.Server as D
import qualified Adapter.InMemory.Server as Mem
import qualified Domain.GameBot.Bot as Bot


wsListener :: WSConnection -> D.SessionId -> App AppState ()
wsListener conn wsId = do
    forever $ do
      msg <- liftIO $ receiveMessage conn
      D.pushInputMessage wsId msg
      D.processMessages Bot.processOneWSMessage wsId


appLoop :: App AppState ()
appLoop = forever $ do
  tvar <- asks getter
  wsIds <- liftIO $ atomically $ do
    ss <- readTVar tvar
    let ids = Mem.serverWSToSend ss
    writeTVar tvar ss{Mem.serverWSToSend = []}  -- TODO make function in Server class for it
    pure ids
  traverse_ D.sendOutMessage wsIds
  liftIO $ threadDelay 10_000


runApp :: Port -> WSTimeout -> IO ()
runApp port timeout = do
  ss <- newTVarIO Mem.initialServerState -- TODO make function in Server class for it

  let r = ss

  let act = \conn wsId -> runSession r (wsListener conn wsId)
  let initConnection = \conn -> runSession r (D.initSession conn (D.RegUserId 666)) -- TODO implement UserId generator
  let disconnect = \conn sId -> runSession r (D.disconnectSession sId)
  _ <- forkIO $ startWebSocketServer port timeout act initConnection disconnect
  putStrLn $ "Starting WebSocket server on port " <> tshow port <> "..."  
  runReaderT (unApp appLoop) r
