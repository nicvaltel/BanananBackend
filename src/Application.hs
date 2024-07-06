{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Avoid lambda" #-}
module Application (runApp) where


import Reexport
import Lib
import WebSocketServer
import qualified Network.WebSockets as WS
import Domain.Session
import Control.Concurrent (threadDelay)
import Network.WebSockets


wsListener :: WSConnection -> WSSessionId -> App ()
wsListener conn wsId = do
    forever $ do
      msg <- liftIO (WS.receiveData conn :: IO Text)
      pushInputMessage wsId msg
      processMessages wsId
    
appLoop :: App ()
appLoop = forever $ do
  tvar <- ask
  wsIds <- liftIO $ atomically $ do
    session <- readTVar tvar
    let ids = sessionWSToSend session
    writeTVar tvar session{sessionWSToSend = []}    
    pure ids
  traverse_ sendOutMessage wsIds
  liftIO $ threadDelay 10_000


webSocketServer :: (WS.Connection -> wsId -> IO ())  -> (WS.Connection -> IO wsId) -> (WS.Connection -> wsId -> IO ()) -> WS.ServerApp
webSocketServer act initConnection disconnect = \pendingConn -> do
  conn <- acceptRequest pendingConn
  wsId <- initConnection conn
  WS.withPingThread conn 30 (pure ()) $ do
    finally
      (act conn wsId)
      (disconnect conn wsId)
  pure ()
  
runApp :: IO ()
runApp = do
  session <- newTVarIO initialSession
  putStrLn "Starting WebSocket server on port 1234..."
  let act = \conn wsId -> runSession session (wsListener conn wsId)
  let initConnection = \conn -> runSession session (initWSSession conn)
  let disconnect = \conn wsId -> runSession session (disconnectWSSession wsId)
  _ <- forkIO $ WS.runServer "0.0.0.0" 1234 (webSocketServer act initConnection disconnect)

  runReaderT (unApp appLoop) session
