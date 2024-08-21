module Utils.Utils where


import ClassyPrelude
import Text.Read (reads)


safeRead :: Read a => String -> Maybe a
safeRead s
  | [(x, "")] <- reads s = Just x -- reads :: Read a => String -> [(a, String)]
  | otherwise = Nothing


splitPlaces :: [Int] -> [a] -> [[a]] 
splitPlaces ns inputxs = reverse $ go ns ([], inputxs)
  where 
    go [] (out,xs) = xs:out
    go (n:restN) (out, xs) =
      case splitAt n xs of
        (ys, []) -> ys:out 
        (ys, rest) -> go restN (ys:out, rest)

