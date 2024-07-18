{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}


module Domain.GameBot.Server where



import Domain.GameBot.Actors 
import Reexport
import WebSocketServer (WSMessage)
import Domain.GameBot.GameModel (AppMod, modgs, mkNewNameId, FlyBall (..))
import Data.ByteString (fromStrict)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Map.Strict as M
import Data.Foldable (for_)



data RemoteMessage = ModelDiffMsg ModelDiff | GameOverMsg
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

data BallPosition = BallPosition {col :: BallColor, x :: Int, y :: Int}
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

data GunPosition = GunPosition {angleSpeed :: Float, angle :: Float}
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

data FlyingBallPosition = FlyingBallPosition {fbcol :: BallColor, startX :: Int, startY :: Int, vx :: Float, vy :: Float}
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

data ModelDiffActors = ModelDiffActors
  { balls :: Maybe [BallPosition],
    flyingBall :: Maybe (Either Int FlyingBallPosition), -- Maybe (Maybe FlyingBallPosition) doesnt work - decodeJson(encodeJson $ Just Nothing) decodes to Nothing, not to Just Nothing. So using Either instead
    gun :: Maybe GunPosition, -- angle speed
    ballQueue :: Maybe BallColor
  } deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)


data ModelDiff = ModelDiff
  { gameIsRunning :: Maybe Bool,
    shotsCounter :: Maybe Int,
    actors :: ModelDiffActors
  } deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)



processOneInputMessage :: WSMessage -> AppMod ()
processOneInputMessage msg = do
  let msgTxt = (fromStrict . encodeUtf8) msg
  case (decode msgTxt :: Maybe RemoteMessage) of
    Just (ModelDiffMsg mDiff) -> processModelDiff mDiff
    Just GameOverMsg -> modgs $ \gs -> gs{gameIsRunning = False}
    _ -> pure ()
  where

    processModelDiff :: ModelDiff -> AppMod ()
    processModelDiff mDiff = do
      maybe (pure ()) updateRemoteBalls (balls $ actors mDiff )
      maybe (pure ()) updateRemoteFlyingBall (flyingBall $ actors mDiff)
      maybe (pure ()) updateRemoteGun (gun $ actors mDiff)
      maybe (pure ()) updateRemoteBallQueue (ballQueue $ actors mDiff)

    updateRemoteBallQueue :: BallColor -> AppMod ()
    updateRemoteBallQueue color =  do
      modgs $ \gs -> 
        gs{remoteActors = gs.remoteActors{ballQueue = 
          gs.remoteActors.ballQueue & \a -> 
            case a.actorActorData of
              ActorBallQueue q -> a{actorActorData = ActorBallQueue q{nextBallColor = color}}
              _ -> a      
        }}


    updateRemoteGun :: GunPosition -> AppMod ()
    updateRemoteGun gunPos = do
      modgs $ \gs -> gs{remoteActors = gs.remoteActors{
          gun = gs.remoteActors.gun & \a ->
            case a.actorActorData of
              ActorGun gun -> a{ actorActorData = ActorGun gun{angleSpeed = gunPos.angleSpeed}
                               , angle = gunPos.angle}
              _ -> a
        }}

    updateRemoteBalls :: [BallPosition] -> AppMod ()
    updateRemoteBalls newBallsPositions = do
      gameSt <- get
      modgs $ \gs -> gs{remoteActors = gs.remoteActors{balls = M.empty}}

      for_ newBallsPositions $ \bp -> do
        nameId <- mkNewNameId
        let newBallActor = Actor {
                  nameId = nameId
                , x = fromIntegral bp.x
                , y = fromIntegral bp.y
                , width = gameSt.gameConfig.ballDiameter
                , height = gameSt.gameConfig.ballDiameter
                , angle = 0.0
                , actorActorData = ActorBall Ball { color = bp.col } 
              } 
        modgs $ \gs -> gs{ remoteActors = gs.remoteActors{balls = M.insert nameId newBallActor gs.remoteActors.balls}}

    updateRemoteFlyingBall :: Either Int FlyingBallPosition -> AppMod ()
    updateRemoteFlyingBall (Left _) = do
      gameSt <- get
      case gameSt.remoteActors.flyingBall of
        Nothing -> pure ()
        Just _ -> modgs $ \gs -> gs{remoteActors = gs.remoteActors{flyingBall = Nothing}}
    
    updateRemoteFlyingBall (Right fb) = do
      gameSt <- get
      nameId <- mkNewNameId
      let newFlyBallActor = Actor {
                  nameId = nameId
                , x = fromIntegral fb.startX
                , y = fromIntegral fb.startY
                , width = gameSt.gameConfig.ballDiameter
                , height = gameSt.gameConfig.ballDiameter
                , angle = 0.0
                , actorActorData = ActorBall Ball { color = fb.fbcol } 
              } 
      modgs $ \gs -> gs{ remoteActors = 
              gs.remoteActors{flyingBall = 
                Just FlyBall{flyball = newFlyBallActor, vx = fb.vx, vy = fb.vy}}} 














