{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant lambda" #-}

module WebSocketServer
  ( WSConnection,
    WSMessage,
    Port,
    WSTimeout,
    WSConnectionAct,
    WSChan (..),
    pushWSIn,
    popWSOut,
    emptyWSChan,
    webSocketServer,
    startWebSocketServer,
    receiveMessage,
    runEcho,
    sendTextData,
  )
where

import qualified Network.WebSockets as WS
import Reexport
import Domain.Server (WSConnection, WSMessage)

type Port = Int

type WSTimeout = Int -- in milliseconds

type WSConnectionAct a = WSConnection -> IO a

data WSChan = WSChan
  { wschanConn :: WSConnection,
    wschanIn :: [WSMessage],
    wschanOut :: [WSMessage]
  }

pushWSIn :: WSMessage -> WSChan -> WSChan
pushWSIn msg chan@WSChan {wschanIn} = chan {wschanIn = msg : wschanIn}

popWSOut :: WSChan -> (WSChan, [WSMessage])
popWSOut chan@WSChan {wschanOut} = (chan {wschanOut = []}, wschanOut)

emptyWSChan :: WSConnection -> WSChan
emptyWSChan conn =
  WSChan
    { wschanConn = conn,
      wschanIn = mempty,
      wschanOut = mempty
    }

instance Show WSChan where
  show WSChan {wschanIn, wschanOut} =
    "WsChanIn: " <> show wschanIn <> "; WsChanOut: " <> show wschanOut

receiveMessage :: WSConnection -> IO WSMessage
receiveMessage = WS.receiveData


webSocketServer ::
  WSTimeout ->
  (WSConnection -> wsid -> IO ()) ->
  (WSConnection -> IO wsid) ->
  (WSConnection -> wsid -> IO ()) ->
  WS.ServerApp
webSocketServer timeout act initConnection disconnect = \pendingConn -> do
  conn <- WS.acceptRequest pendingConn
  wsId <- initConnection conn
  WS.withPingThread conn timeout (pure ()) $ do -- default timeout = 30ms
    finally
      (act conn wsId)
      (disconnect conn wsId)

startWebSocketServer ::
  Port ->
  WSTimeout ->
  (WSConnection -> wsid -> IO ()) ->
  (WSConnection -> IO wsid) ->
  (WSConnection -> wsid -> IO ()) ->
  IO ()
startWebSocketServer port timeout act initConnection disconnect = do
  WS.runServer "0.0.0.0" port (webSocketServer timeout act initConnection disconnect)

sendTextData :: WSConnection -> Text -> IO ()
sendTextData = WS.sendTextData


-- WS handle connection exceptions
catchConnectionException :: WS.Connection -> IO ()
catchConnectionException conn = do
  catch
    (WS.receiveDataMessage conn >> pure ()) -- Attempt to receive data (use your specific logic here)
    (\(_ :: WS.ConnectionException) -> pure ()) -- Handle connection exceptions



------------ SIMPLE WebSocketServer Example -----------

simpleWebSocketServer ::
  (WS.Connection -> IO ()) ->
  WS.ServerApp
simpleWebSocketServer act = \pendingConn -> do
  conn <- WS.acceptRequest pendingConn
  WS.withPingThread conn 30 (pure ()) $ do
    finally
      (act conn)
      (disconnect conn)
      where disconnect conn = pure ()

startSimpleWebSocketServer ::
  Port ->
  (WS.Connection -> IO ()) ->
  IO ()
startSimpleWebSocketServer port act = do
  WS.runServer "0.0.0.0" port (simpleWebSocketServer act)

echo :: WS.Connection -> IO ()
echo conn = forever $ do
  msg <- WS.receiveData conn :: IO Text
  putStrLn msg
  WS.sendTextData conn msg
-- WS.sendTextData conn $ msg `T.append` ", meow"

runEcho :: IO ()
runEcho = do
  putStrLn "Starting WebSocket server on port 1234..."
  startSimpleWebSocketServer 1234 echo 
