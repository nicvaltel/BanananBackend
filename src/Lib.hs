{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import Reexport
import Domain.Session
import qualified Adapter.InMemory.Session as M
import qualified Adapter.InMemory.WSServ as M
import Adapter.InMemory.Type(MemState)
import qualified Prelude
import Domain.GameBot.GameModel (GameState)


newtype App a = App { unApp :: ReaderT (MemState GameState) IO a  } deriving (Functor, Applicative, Monad, MonadReader (MemState GameState), MonadIO, MonadFail)


instance SessionRepo App where
  newGuestSession = App M.newGuestSession
  newUserSession = App . M.newUserSession
  findUserIdBySessionId = App . M.findUserIdBySessionId
  findGuestIdBySessionId = App . M.findGuestIdBySessionId
  deleteUserSession = App . M.deleteUserSession
  deleteGuestSession = App . M.deleteGuestSession

instance WSServ App where
  initWSSession = App . M.initWSSession
  disconnectWSSession = App . M.disconnectWSSession
  sendOutMessage = App . M.sendOutMessage
  pushInputMessage wsId msg = App $ M.pushInputMessage wsId msg
  processMessages = App . M.processMessages

instance Bot App where
  -- processWSMessage :: WSMessage -> gs -> m (gs, WSMessage)
  processWSMessage msg gs = pure (gs, msg)
    
    -- App $ do
    --   st :: GameState <- get
    --   let (msgOut, newSt) = runState (BOT.processWSMessage msg) st
    --   put newSt
    --   pure msgOut


runSession :: MemState GameState -> App a -> IO a
runSession state = flip runReaderT state . unApp
  
runRoutine :: App () -> IO ()
runRoutine routine = do 
  session <- newTVarIO initialSession
  runSession session routine
  pure ()

routine' :: App ()
routine' = do
  (gId1, sId1) <- newGuestSession
  (gId2, sId2) <- newGuestSession
  gf1 <- findGuestIdBySessionId sId1
  (gId3, sId3) <- newGuestSession
  deleteGuestSession sId2
  gf3 <- findGuestIdBySessionId sId3
  gf2 <- findGuestIdBySessionId sId2
  deleteGuestSession sId1
  gf1' <- findGuestIdBySessionId sId1
  gf3' <- findGuestIdBySessionId sId3
  let results = 
        [ show gId1,
          show sId1,
          show gId2,
          show sId2,
          show gf1,
          show gId3,
          show sId3,
          show gf3,
          show gf2,
          show gf1',
          show gf3'
        ]
  liftIO $ traverse_ Prelude.putStrLn results

