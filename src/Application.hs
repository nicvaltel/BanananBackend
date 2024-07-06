{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Avoid lambda" #-}
module Application (runApp) where


import Reexport
import Lib
import WebSocketServer
import Domain.Session
import Control.Concurrent (threadDelay)

wsListener :: WSConnection -> WSSessionId -> App ()
wsListener conn wsId = do
    forever $ do
      msg <- liftIO $ receiveMessage conn
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


runApp :: Port -> WSTimeout -> IO ()
runApp port timeout = do
  session <- newTVarIO initialSession

  let act = \conn wsId -> runSession session (wsListener conn wsId)
  let initConnection = \conn -> runSession session (initWSSession conn)
  let disconnect = \conn wsId -> runSession session (disconnectWSSession wsId)
  _ <- forkIO $ startWebSocketServer port timeout act initConnection disconnect
  putStrLn $ "Starting WebSocket server on port " <> tshow port <> "..."  

  runReaderT (unApp appLoop) session
