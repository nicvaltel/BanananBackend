{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module GameBot.Actors 
  ( Gun(..)
  , Ball(..)
  , Dragon(..)
  , BallQueue(..)
  , gunMock
  , BallColor
  , colorFromRandomInt
  , dragonMock
  , ballQueueMock
  , ActorData(..)
  , NameId(..)
  , Actor(..)
  , actorMock
  ) where


import Reexport

newtype NameId = NameId {unNameId :: String} deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON, FromJSONKey, ToJSONKey)

data Actor ac = Actor {
    nameId :: NameId
  , x :: Float
  , y :: Float
  , width :: Float
  , height :: Float
  , angle :: Float
  , actorActorData :: ac
} deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

actorMock :: Actor () 
actorMock = Actor
  { nameId = NameId "actor_mock",
    x = 0.0,
    y = 0.0,
    width = 0.0,
    height = 0.0,
    angle = 0.0,
    actorActorData = ()
  }

data Gun = Gun {
    angleSpeed :: Float
  , maxAngleSpeed :: Float
  , maxLeftAngle :: Float
  , maxRightAngle :: Float
} deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

gunMock :: Gun 
gunMock = Gun
  {
    angleSpeed = 0.0
  , maxAngleSpeed = 0.0
  , maxLeftAngle = 0.0
  , maxRightAngle = 0.0
  }


data BallColor = Red | Green | Blue | Yellow | Purple 
  deriving (Show, Eq, Ord, Bounded, Enum, Generic, FromJSON, ToJSON)

colorFromRandomInt :: Int -> BallColor
colorFromRandomInt n = case n `mod` 5 of
  0 -> Red
  1 -> Green
  2 -> Blue
  3 -> Yellow
  _ -> Purple 

newtype Ball = Ball{ color :: BallColor}  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

newtype Dragon = Dragon { dragonAnimation :: String } deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

dragonMock :: Dragon
dragonMock = Dragon {dragonAnimation = ""}

data BallQueue = BallQueue
  { nextBallColor :: BallColor
  , ballQueueAnimation :: String
  } deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)


ballQueueMock :: BallQueue
ballQueueMock = BallQueue {nextBallColor = Red, ballQueueAnimation = ""}

data ActorData = 
    ActorGun Gun
  | ActorBall Ball
  | ActorDragon Dragon
  | ActorBallQueue BallQueue 
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)