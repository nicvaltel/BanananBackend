{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Adapter.InMemory.Type where


import Reexport
import Domain.Session
import Data.Has (Has)
import Domain.GameBot.GameModel (GameState)

-- type MemState gs = (TVar (Session gs), Map WSSessionId (TVar gs))

type InMemory r m a = 
  ( MonadIO m,
    Has (TVar Session) r, 
    Has (Map WSSessionId (TVar GameState)) r) => 
  ReaderT r m a