module Main (main) where

import Reexport
import qualified Data.ByteString as B
import qualified Application

import Config

configPath :: FilePath
configPath = "./config.json"

main :: IO ()
main = do
  rawConfig <- B.readFile configPath
  case decodeStrict rawConfig of
    Just Config{port, wstimeoutMs, wsThreadDelayMs} -> Application.runApp port wstimeoutMs wsThreadDelayMs
    Nothing -> putStrLn $ "Invalid config file " <> tshow configPath
