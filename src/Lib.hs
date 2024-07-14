{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module Lib where

import Reexport
import Domain.Session
import qualified Adapter.InMemory.Session as M
import qualified Adapter.InMemory.WSServ as M
import qualified Prelude
import Domain.GameBot.GameModel (GameState)
import qualified Data.Map.Strict as Map


type LibState = (TVar Session, Map WSSessionId (TVar GameState))

newtype App r a = App { unApp :: ReaderT LibState IO a  } deriving (Functor, Applicative, Monad, MonadReader LibState, MonadIO, MonadFail)


instance SessionRepo (App LibState) where
  newGuestSession = App M.newGuestSession
  newUserSession = App . M.newUserSession
  findUserIdBySessionId = App . M.findUserIdBySessionId
  findGuestIdBySessionId = App . M.findGuestIdBySessionId
  deleteUserSession = App . M.deleteUserSession
  deleteGuestSession = App . M.deleteGuestSession

instance WSServ (App LibState) where
  initWSSession = App . M.initWSSession
  disconnectWSSession = App . M.disconnectWSSession
  sendOutMessage = App . M.sendOutMessage
  pushInputMessage wsId msg = App $ M.pushInputMessage wsId msg
  processMessages f = App . M.processMessages f

-- instance Bot (App LibState) where
--   -- processWSMessage :: WSMessage -> gs -> m (gs, WSMessage)
--   processWSMessage msg gs = pure (gs, msg)
    
    -- App $ do
    --   st :: GameState <- get
    --   let (msgOut, newSt) = runState (BOT.processWSMessage msg) st
    --   put newSt
    --   pure msgOut


runSession :: LibState -> App LibState a -> IO a
runSession state = flip runReaderT state . unApp
  
runRoutine :: App LibState () -> IO ()
runRoutine routine = do 
  session <- newTVarIO initialSession
  let gameStates = Map.empty
  runSession (session, gameStates) routine
  pure ()

routine' :: App LibState ()
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

