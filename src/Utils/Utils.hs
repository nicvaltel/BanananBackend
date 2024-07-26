{-# LANGUAGE ScopedTypeVariables #-}
module Utils.Utils where


import ClassyPrelude
import Text.Read (reads)


safeRead :: Read a => String -> Maybe a
safeRead s
  | [(x, "")] <- reads s = Just x -- reads :: Read a => String -> [(a, String)]
  | otherwise = Nothing