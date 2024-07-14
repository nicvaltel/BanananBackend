{-# LANGUAGE RankNTypes #-}

module Adapter.InMemory.Type where


import Reexport
import Domain.Session

type MemState gs = TVar (Session gs)
type InMemory gs m a = MonadIO m => ReaderT (MemState gs) m a