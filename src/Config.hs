module Config where

import Reexport

data Config = Config 
  { port :: Int
  , wstimeoutMs :: Int
  , wsThreadDelayMs :: Int
  } deriving (Show)


instance FromJSON Config where
  parseJSON (Object config) = do
    port <- config .: "port"
    wstimeoutMs <- config .: "wstimeoutMs"
    wsThreadDelayMs <- config .: "wsThreadDelayMs"
    pure Config{port, wstimeoutMs, wsThreadDelayMs}
  parseJSON _ = mzero