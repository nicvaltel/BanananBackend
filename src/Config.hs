module Config where

import Reexport
import GHC.Generics (Generic)


data Config = Config 
  { port :: Int
  , wstimeoutMs :: Int
  } deriving (Show)


instance FromJSON Config where
  parseJSON (Object config) = do
    port <- config .: "port"
    wstimeoutMs <- config .: "wstimeoutMs"
    pure Config{port, wstimeoutMs}
  parseJSON _ = mzero