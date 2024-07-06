{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}

module WebSocketServer 
  ( WSConnection
  , WSMessage
  , Port
  , WSConnectionAct
  , WSChan(..)
  , pushWSIn
  , popWSOut
  , emptyWSChan

  ) where

import Reexport
import qualified Network.WebSockets as WS
import Network.WebSockets

type WSConnection = WS.Connection
type WSMessage = Text
type Port = Int

type WSConnectionAct a = WS.Connection -> IO a 

data WSChan = WSChan
  { wschanConn :: WSConnection
  , wschanIn :: [WSMessage]
  , wschanOut :: [WSMessage]
  }

pushWSIn :: WSMessage -> WSChan -> WSChan
pushWSIn msg chan@WSChan{wschanIn} = chan{wschanIn = msg : wschanIn}

popWSOut :: WSChan -> (WSChan, [WSMessage])
popWSOut chan@WSChan{wschanOut} = (chan{wschanOut = []}, wschanOut)

emptyWSChan :: WSConnection -> WSChan
emptyWSChan conn = WSChan
  { wschanConn = conn
  , wschanIn = mempty
  , wschanOut = mempty
  }

instance Show WSChan where
  show WSChan{wschanIn, wschanOut} = 
    "WsChanIn: " <> show wschanIn <> "; WsChanOut: " <> show wschanOut 




-- Define the WebSocket application
webSocketServer :: WSConnectionAct () -> WSConnectionAct a -> WSConnectionAct () -> WS.ServerApp
webSocketServer act initConnection disconnect = \pendingConn -> do
  conn <- acceptRequest pendingConn
  initConnection conn
  WS.withPingThread conn 30 (pure ()) $ do
    finally
      (act conn)
      (disconnect conn)
  
  -- where
  --   disconnect conn = do
  --     putStrLn $ tshow conn <> " disconnected"
  --     pure ()
  --     -- removeConnection wSState idConn
  --     -- logger LgInfo $ show idConn ++ " disconnected"
    
  --   initConnection conn = do
  --     putStrLn $ tshow conn <> " WebSocket connection established."
  --     pure ()


-- Function to handle connection exceptions
catchConnectionException :: Connection -> IO ()
catchConnectionException conn = do
  catch
    (receiveDataMessage conn >> pure ()) -- Attempt to receive data (use your specific logic here)
    (\(_ :: ConnectionException) -> pure ()) -- Handle connection exceptions

-- Function to start the WebSocket server
startWebSocketServer :: Port -> WSConnectionAct () -> WSConnectionAct a -> WSConnectionAct () -> IO ()
startWebSocketServer port act  initConnection disconnect = do
  WS.runServer "0.0.0.0" port (webSocketServer act  initConnection disconnect)



echo :: WS.Connection -> IO ()
echo conn = forever $ do
    msg <- WS.receiveData conn :: IO Text
    putStrLn msg
    WS.sendTextData conn msg
    -- WS.sendTextData conn $ msg `T.append` ", meow"

runEcho :: IO ()
runEcho = do
  putStrLn "Starting WebSocket server on port 1234..."
  startWebSocketServer 1234 echo (const $ pure ()) (const $ pure ())


-- makePendingConnection :: Socket -> ConnectionOptions -> IO PendingConnection 

-- runWebSocketServer :: String -> Int -> IO ()
-- runWebSocketServer host port = do
--     -- (state :: db WebSocketServerState) <- newContainerVar newWebSocketServerState
--     -- (gameRoomsMap :: db RoomsMap) <- createGameRoomsRepo
--     WS.runServer host port $ webSocketServer gameRoomsMap state
--     pure state


-- webSocketServer :: WS.ServerApp
-- webSocketServer = \pending -> do
--   conn <- WS.acceptRequest pending
--   -- logger LgInfo $ show idConn ++ " connected"
--   WS.withPingThread conn 30 (pure ()) $ do
--     finally
--       (runRWST (wsThreadMessageListner idConn) (ConnThreadReader conn gameRoomsMap) (ConnThreadState Nothing) >> pure ())
--       (disconnect idConn)
--   where
--     disconnect idConn = do
--       removeConnection wSState idConn
--       logger LgInfo $ show idConn ++ " disconnected"

