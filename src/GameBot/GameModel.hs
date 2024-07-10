{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module GameBot.GameModel where


import Reexport
import GameBot.Actors
import Data.Map.Strict as M
import GameBot.GraphBall


data Board = Board
  { width  :: Float
  , height :: Float
  } deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

data GSLastRowsAdded = GSLastRowsAdded { time :: Double, numberOfBalls :: Int}
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

data FlyBall = FlyBall {flyball :: Actor ActorData, vx :: Float, vy :: Float}
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

data GSActors = GSActors
  { balls :: M.Map NameId (Actor ActorData)
  , flyingBall :: Maybe FlyBall
  , gun :: Actor ActorData
  , dragon :: Actor ActorData
  , ballQueue :: Actor ActorData
  } deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

data GSBoards = GSBoards {board :: Board, remoteBoard :: Board}
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

data GameState = GameState {
      gameStepNumber :: Int
    , score :: Int
    , canvasWidth :: Float
    , canvasHeight :: Float
    , ballSpeed :: Float
    , gameIsRunning :: Bool
    , shotsCounter :: Int
    , lastRowsAdded :: GSLastRowsAdded
    , actors :: GSActors
    , remoteActors :: GSActors
    , graphBall :: GraphBall
    , boards :: GSBoards
  }

-- modgs :: (GameState -> GameState) -> AppGame Unit
-- modgs f = modmod $ \mr -> let (GameState g) = mr.game in mr { game = GameState (f g) }
