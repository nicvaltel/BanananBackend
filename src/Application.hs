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
import qualified Adapter.HTTP.Main as HTTP
import Data.Either.Extra(mapLeft)
import Utils.Utils (splitPlaces)
import qualified WebSocketServer as WS

type WSThreadDelayMs = Int -- in milliseconds

wsListener :: WSConnection -> WSChan -> IO ()
wsListener conn wsChan = do
    forever $ do
      msg <- liftIO $ receiveMessage conn
      atomically $ WS.pushWSIn wsChan msg
      -- D.pushInputWSMessage sId gId msg
      -- D.processWSMessages D.processOneWSMessageEcho sId gId


-- appLoopExample :: WSThreadDelayMs -> App ()
-- appLoopExample wsThreadDelayMs = forever $ do
--   D.sendOutAllWSMessages
--   liftIO $ threadDelay wsThreadDelayMs -- e.g. 10_000 ms

-- TODO add fokrIO for processing all ws chans
runApp :: Port -> WSTimeout -> IO ()
runApp port wstimeout = do
  ss <- newTVarIO Mem.initialServerState
  let r = ss
  let act = \conn sId wsChan -> wsListener conn wsChan
  let initConnection = \conn sId mayUid -> runSession r (mapLeft tshow <$> D.newSession mayUid sId conn)
  let disconnect = \conn sId -> runSession r (D.disconnectWSConn conn sId)
  _ <- forkIO $ WS.startWebSocketServer port wstimeout extractFromRequestPath act initConnection disconnect
  putStrLn $ "Starting WebSocket server on port " <> tshow port <> "..."
  let appRunner =  runSession r
  HTTP.main 3000 appRunner
 



-- TODO This module is not a place for this function, but I don't know where to place it...?
extractFromRequestPath :: ByteString -> Maybe (D.SessionId, Maybe D.UserId)
extractFromRequestPath bstr = do
  let str  = (unpack . decodeUtf8) bstr :: String
  let lenSid = length ("/session?sessionId=" :: String)
  let lenUid = length ("&userId=" :: String)
  case splitPlaces [lenSid, D.lengthOfSessionIdConst, lenUid, 10000] str of
    ["/session?sessionId=", sId, "&userId="] -> Just (D.SessionId $ pack sId, Nothing ) 

    ["/session?sessionId=", sId, "&userId=", uIdString] ->
      case safeRead uIdString :: Maybe Int of
        Just uId -> Just (D.SessionId $ pack sId, Just $ D.UserId uId ) 
        _ -> Nothing

    _ -> Nothing


-- -- TODO This module is not a place for this function, but I don't know where to place it...?
-- extractFromRequestPath :: ByteString -> Maybe D.SessionId
-- extractFromRequestPath bstr = do
--   let str  = (unpack . decodeUtf8) bstr :: String
--   let lenSid = length ("/session?sessionId=" :: String)
--   case splitAt lenSid str of
--     ("/session?sessionId=", numStr) -> D.SessionId <$> safeRead numStr
--     _ -> Nothing
