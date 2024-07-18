{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
module Domain.User where

import Reexport


data UserKind = Reg | Guest | Bot

newtype UserId (u :: UserKind) = UserId Int
  deriving (Show, Eq, Ord)