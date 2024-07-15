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
import Data.Has (Has(getter))
import qualified Data.Map.Strict as Map
import Domain.GameBot.Bot (processOneWSMessage)

wsListener :: WSConnection -> WSSessionId -> App LibState ()
wsListener conn wsId = do
    forever $ do
      msg <- liftIO $ receiveMessage conn
      pushInputMessage wsId msg
      processMessages processOneWSMessage wsId

    
appLoop :: App LibState ()
appLoop = forever $ do
  tvar <- asks getter
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

  let r = (session, Map.empty)

  let act = \conn wsId -> runSession r (wsListener conn wsId)
  let initConnection = \conn -> runSession r (initWSSession conn)
  let disconnect = \conn wsId -> runSession r (disconnectWSSession wsId)
  _ <- forkIO $ startWebSocketServer port timeout act initConnection disconnect
  putStrLn $ "Starting WebSocket server on port " <> tshow port <> "..."  


  runReaderT (unApp appLoop) r
