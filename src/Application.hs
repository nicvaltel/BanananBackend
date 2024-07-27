{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Avoid lambda" #-}
module Application (runApp) where


import Reexport
import ClassyPrelude
import Lib
import WebSocketServer
import Control.Concurrent (threadDelay)
import qualified Domain.Server as D
import qualified Adapter.InMemory.Server as Mem
import qualified Domain.GameBot.Bot as Bot
import qualified Adapter.HTTP.Main as HTTP


type WSThreadDelayMs = Int -- in milliseconds

wsListener :: WSConnection -> D.SessionId -> App ()
wsListener conn wsId = do
    forever $ do
      msg <- liftIO $ receiveMessage conn
      D.pushInputWSMessage wsId msg
      D.processWSMessages Bot.processOneWSMessage wsId


appLoopExample :: WSThreadDelayMs -> App ()
appLoopExample wsThreadDelayMs = forever $ do
  D.sendOutAllWSMessages
  liftIO $ threadDelay wsThreadDelayMs -- e.g. 10_000 ms


runApp :: Port -> WSTimeout -> IO ()
runApp port wstimeout = do
  ss <- newTVarIO Mem.initialServerState
  let r = ss
  let act = \conn wsId -> runSession r (wsListener conn wsId)
  let initConnection = \conn wsId -> runSession r (D.initWSConn conn wsId) -- TODO implement initSession not only for Guests, but for Reg users
  let disconnect = \conn wsId -> runSession r (D.disconnectWSConn conn wsId)
  _ <- forkIO $ startWebSocketServer port wstimeout extractSessionIdFromRequestPath act initConnection disconnect
  putStrLn $ "Starting WebSocket server on port " <> tshow port <> "..."
  let appRunner =  runSession r
  HTTP.main 3000 appRunner
  -- runReaderT (unApp (appLoop wsThreadDelayMs)) r
  

-- TODO This module is not a place for this function, but I don't know where to place it...?
extractSessionIdFromRequestPath :: ByteString -> Maybe D.SessionId
extractSessionIdFromRequestPath bstr = do
  let str  = (unpack . decodeUtf8) bstr :: String
  case splitAt 19 str of
    ("/session?sessionId=", numStr) -> D.SessionId <$> safeRead numStr
    _ -> Nothing