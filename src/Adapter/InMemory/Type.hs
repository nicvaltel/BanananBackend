{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Adapter.InMemory.Type where


import Reexport
import Domain.Session
import Data.Has (Has)

type InMemory r m a = ( MonadIO m, Has (TVar Session) r) => ReaderT r m a