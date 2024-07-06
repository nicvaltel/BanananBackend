{-# LANGUAGE RankNTypes #-}

module Adapter.InMemory.Type where


import Reexport
import Domain.Session

type MemState = TVar Session
type InMemory m a = MonadIO m => ReaderT MemState m a