module Main (main) where

import Prelude(IO, putStrLn)
import qualified Application
import qualified WebSocketServer as Application

main :: IO ()
main = do
  Application.runApp 1234 30 -- port timeout(ms)
