{-# LANGUAGE NumericUnderscores #-}
module Application (runApp) where


import Reexport
import Lib
import WebSocketServer
import qualified Network.WebSockets as WS
import Domain.Session
import Control.Concurrent (threadDelay)



wsListener :: WSConnection -> App ()
wsListener conn = do
    wsId <- initWSSession conn
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

  


runApp :: IO ()
runApp = do
  session <- newTVarIO initialSession
  putStrLn "Starting WebSocket server on port 1234..."
  _ <- forkIO $ startWebSocketServer 1234 (\conn -> runReaderT (unApp $ wsListener conn) session )
  runReaderT (unApp appLoop) session
