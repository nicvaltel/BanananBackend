{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module Adapter.InMemory.Type where


import Reexport
import Domain.Session
import Data.Has (Has)

type InMemory r m = ( MonadIO m, Has (TVar Session) r, MonadReader r m)