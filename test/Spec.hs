import Reexport

import GameBot.Actors


-- For showing print output use
-- cabal new-test --test-show-details=streaming
main :: IO ()
main = do
  testBallColorJSON


testBallColorJSON :: IO ()
testBallColorJSON = do
  let colors = [minBound .. maxBound ] :: [BallColor] 
  let jsons = [encode c | c <- colors]
  let colors' = map decode jsons :: [Maybe BallColor]
  print jsons
  print colors'
  let res = and [ Just c == mbC | (c,mbC) <- zip colors colors']
  print res
  
