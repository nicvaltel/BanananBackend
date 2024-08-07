module Domain.Game
  ( GameType (..),
  )
where

import ClassyPrelude

data GameType = GameType {gameTypeRules :: Int, gameTypeRated :: Bool}
  deriving (Show, Eq, Ord)
