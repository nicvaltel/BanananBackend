{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Avoid lambda" #-}
module Application (runApp) where


import Reexport
import Lib
import WebSocketServer
import Control.Concurrent (threadDelay)
import qualified Domain.Server as D
import qualified Adapter.InMemory.Server as Mem
import qualified Domain.GameBot.Bot as Bot


type WSThreadDelayMs = Int -- in milliseconds

wsListener :: WSConnection -> D.SessionId -> App AppState ()
wsListener conn wsId = do
    forever $ do
      msg <- liftIO $ receiveMessage conn
      D.pushInputMessage wsId msg
      D.processMessages Bot.processOneWSMessage wsId


appLoop :: WSThreadDelayMs -> App AppState ()
appLoop wsThreadDelayMs = forever $ do
  D.sendOutAllMessages
  liftIO $ threadDelay wsThreadDelayMs -- e.g. 10_000 ms


runApp :: Port -> WSTimeout -> WSThreadDelayMs -> IO ()
runApp port timeout wsThreadDelayMs = do
  ss <- newTVarIO Mem.initialServerState

  let r = ss

  let act = \conn wsId -> runSession r (wsListener conn wsId)
  let initConnection = \conn -> runSession r (D.initSession conn (D.RegUserId 666)) -- TODO implement UserId generator
  let disconnect = \conn sId -> runSession r (D.disconnectSession sId)
  _ <- forkIO $ startWebSocketServer port timeout act initConnection disconnect
  putStrLn $ "Starting WebSocket server on port " <> tshow port <> "..."  
  runReaderT (unApp (appLoop wsThreadDelayMs)) r
  