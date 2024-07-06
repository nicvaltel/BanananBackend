module Main (main) where

import Prelude(IO, putStrLn)
import qualified WebSocketServer as W
import qualified Lib
import qualified Application

main :: IO ()
main = do
  Application.runApp
  -- Lib.runRoutine
  -- W.main
  putStrLn "Done"