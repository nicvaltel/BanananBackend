module Domain.GameBot.Bot 
  ( processOneWSMessage
  , processOneWSMessageEcho) where

import Reexport
import ClassyPrelude
import WebSocketServer (WSMessage)
import Domain.GameBot.GameModel (AppMod, GameState)
import qualified Domain.Server as D

type BoxWidth = Float
type BoxHeight = Float
type Diameter = Float
type Y = Float


processOneWSMessage :: [WSMessage] -> WSMessage -> State GameState [WSMessage]
processOneWSMessage outMsgs msg = do
  gs <- get
  put gs
  pure (msg : outMsgs)


processOneWSMessageEcho :: D.SessionId -> WSMessage -> WSMessage
processOneWSMessageEcho sId msg = msg












-- addRandomBalls :: GameConfig -> Int -> BoxWidth -> Y -> AppGame Unit
-- addRandomBalls gameConf n width y = do
--   let xOffset = (width - (toNumber n) * gameConf.ballDiameter) / 2.0
--   let nearRange = gameConf.ballDiameter * gameConf.nearestBallDiameterFactor
--   let 
--     findNearesBalls ::  GameActor -> List GameActor -> List GameActor
--     findNearesBalls ball allBalls = ballsIntersection nearRange ball allBalls


--   for_ (range 0 (n - 1)) $ \i -> do
--     nameId <- mkNewNameId
--     randN :: Int <- getRandom
--     let color = colorFromRandomInt randN
--     let newBallActor = Actor
--           {
--             nameId : nameId
--           , x : xOffset + (toNumber i * gameConf.ballDiameter)
--           , y : y
--           , width : gameConf.ballDiameter
--           , height : gameConf.ballDiameter
--           , z : 1
--           , visible : true
--           , angle : 0.0
--           , cssClass : cssClassOfColor color
--           , imageSource : selectBallQueueImageSource gameConf color
--           , htmlElement : Nothing
--           , data : ActorBall { color : color } 
--           }
--     game <- getGameRec <$> get
--     let neighbours = findNearesBalls newBallActor (M.values game.actors.balls)
--     let newGraphBall = addNodeBall newBallActor neighbours game.graphBall
--     modmod $ \mr -> mr{ act { recentlyAddedActors = {nameId : nameId, parentElemId : gameConf.boards.boardElementId , clue : "ActorBall"} : mr.act.recentlyAddedActors }}
--     modgs $ \gs -> gs{graphBall = newGraphBall}
--     modgs $ \gs -> gs{ actors{balls = M.insert nameId newBallActor gs.actors.balls}}
