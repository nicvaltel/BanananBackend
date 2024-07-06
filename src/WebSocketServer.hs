{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}

module WebSocketServer where

import Reexport
import qualified Network.WebSockets as WS
import Network.WebSockets

type WSConnection = WS.Connection
type WSMessage = Text

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


meow :: WS.Connection -> IO ()
meow conn = forever $ do
    msg <- WS.receiveData conn :: IO Text
    putStrLn msg
    WS.sendTextData conn msg
    -- WS.sendTextData conn $ msg `T.append` ", meow"


-- Define the WebSocket application
app :: (WS.Connection -> IO ()) -> WS.ServerApp
app act = \pendingConn -> do
  conn <- acceptRequest pendingConn
  putStrLn "WebSocket connection established."
  
  act conn
  -- Perform your WebSocket-specific logic here
  -- For example, you can use 'sendTextData' and 'receiveData' to communicate with the client.
  
  -- Ensure the connection is properly closed when the action is complete
  finally
    (do
      putStrLn "WebSocket connection closed."
      sendClose conn ("Closing connection" :: WSMessage))
    (catchConnectionException conn)

-- Function to handle connection exceptions
catchConnectionException :: Connection -> IO ()
catchConnectionException conn = do
  catch
    (receiveDataMessage conn >> pure ()) -- Attempt to receive data (use your specific logic here)
    (\(_ :: ConnectionException) -> pure ()) -- Handle connection exceptions

-- Function to start the WebSocket server
startWebSocketServer :: Int -> (WS.Connection -> IO ()) -> IO ()
startWebSocketServer port act = do
  WS.runServer "0.0.0.0" port (app act)

main :: IO ()
main = do
  putStrLn "Starting WebSocket server on port 1234..."
  startWebSocketServer 1234 meow





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

