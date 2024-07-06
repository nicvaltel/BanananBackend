module Domain.User
  ( UserId (..)
  , GuestId(..)
  , AnyUser(..)
  ) where

import Reexport

newtype UserId = UserId {unUserId :: Int} deriving (Show, Eq, Ord)

newtype GuestId = GuestId {unGuestId :: Int} deriving (Show, Eq, Ord)


data AnyUser = 
    Guest GuestId
  | RegUser UserId
  deriving (Show, Eq, Ord)

