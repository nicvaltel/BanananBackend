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
    popAllWSOut,
  )
where


import ClassyPrelude
import qualified Network.WebSockets as WS

type WSConnection = WS.Connection
type WSMessage = Text
type Port = Int
type WSTimeout = Int -- in milliseconds
type WSConnectionAct a = WSConnection -> IO a

data WSChan = WSChan
  { wschanConn :: WSConnection,
    wschanIn :: TQueue WSMessage,
    wschanOut :: TQueue WSMessage
  }

pushWSIn :: WSChan -> WSMessage -> STM ()
pushWSIn wsChan msg = writeTQueue (wschanIn wsChan) msg

popWSOut :: WSChan -> STM WSMessage
popWSOut wsChan = readTQueue (wschanOut wsChan)

popAllWSOut :: WSChan -> STM [WSMessage]
popAllWSOut wsChan = go []
  where
    go acc = do
      isEmpty <- isEmptyTQueue (wschanOut wsChan)
      if isEmpty
        then pure (reverse acc)
        else do
          msg <- readTQueue (wschanOut wsChan)
          go (msg : acc)


emptyWSChan :: WSConnection -> STM WSChan
emptyWSChan conn = do
  cin <- newTQueue
  cout <- newTQueue
  pure WSChan
    { wschanConn = conn,
      wschanIn = cin,
      wschanOut = cout
    }


receiveMessage :: WSConnection -> IO WSMessage
receiveMessage = WS.receiveData


webSocketServerAlternative ::
  WSTimeout ->
  (ByteString -> Maybe wsid) ->
  (WSConnection -> wsid -> IO ()) ->
  (WSConnection -> wsid -> IO (Either Text ())) ->
  (WSConnection -> wsid -> IO ()) ->
  WS.ServerApp
webSocketServerAlternative wstimeout extractSessionIdFromRequestPath act initConnection disconnect = \pendingConn -> do
  
  let req = WS.pendingRequest pendingConn
  let path = WS.requestPath req
  putStrLn $ "\npath = " ++ tshow path
  case extractSessionIdFromRequestPath path of
    Nothing -> WS.rejectRequest pendingConn "Websocket connection rejected: no SessionId provided"
    Just wsId -> do
      conn <- WS.acceptRequest pendingConn
      eitherWsId <- initConnection conn wsId
      case eitherWsId of
        Left errMsg -> do
          sendTextData conn errMsg
          pure ()
        Right _ -> do
          WS.withPingThread conn wstimeout (pure ()) $ do -- default timeout = 30ms
            finally
              (act conn wsId)
              (disconnect conn wsId)


webSocketServer ::
  WSTimeout ->
  (ByteString -> Maybe (sid, Maybe uid)) -> -- extractFromRequestPath
  (WSConnection -> sid -> WSChan -> IO ()) -> -- act
  (WSConnection -> sid -> Maybe uid -> IO (Either Text WSChan)) -> -- initConnection 
  (WSConnection -> sid -> IO ()) -> -- disconnect
  WS.ServerApp
webSocketServer wstimeout extractFromRequestPath act initConnection disconnect = \pendingConn -> do
  let req = WS.pendingRequest pendingConn
  let path = WS.requestPath req
  -- let headers = WS.requestHeaders req
  -- putStrLn $ "\nheaders = " ++ tshow headers
  -- putStrLn $ "\npath = " ++ tshow path
  conn <- WS.acceptRequest pendingConn
  case extractFromRequestPath path of
    Nothing -> sendTextData conn "Websocket connection rejected: no SessionId provided" -- close connection
    Just (sId, mayUid) -> do
        eitherWSChan <- initConnection conn sId mayUid
        case eitherWSChan of
          Left errMsg -> sendTextData conn errMsg -- close connection
          Right wsChan -> do
            WS.withPingThread conn wstimeout (pure ()) $ do -- default timeout = 30ms
              finally
                (act conn sId wsChan)
                (disconnect conn sId)

startWebSocketServer ::
  Port ->
  WSTimeout ->
  (ByteString -> Maybe (sid, Maybe uid)) -> -- extractSessionIdFromRequestPath
  (WSConnection -> sid -> WSChan -> IO ()) -> -- act
  (WSConnection -> sid -> Maybe uid -> IO (Either Text WSChan)) -> -- initConnection
  (WSConnection -> sid -> IO ()) -> -- disconnect
  IO ()
startWebSocketServer port wstimeout extractFromRequestPath act initConnection disconnect = do
  WS.runServer "0.0.0.0" port (webSocketServer wstimeout extractFromRequestPath act initConnection disconnect)

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
