{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Domain.GameBot.GameModel where


import Reexport
import Domain.GameBot.Actors
import Domain.GameBot.GraphBall
import qualified Data.Map.Strict as M


type AppMod a = State GameState a
  

data Board = Board
  { width  :: Float
  , height :: Float
  } deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

emptyBoard :: Board
emptyBoard = Board 0 0 

data GSLastRowsAdded = GSLastRowsAdded { time :: Double, numberOfBalls :: Int}
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

emptyGSLastRowsAdded :: GSLastRowsAdded
emptyGSLastRowsAdded = GSLastRowsAdded {time  = 0, numberOfBalls = 0}

data FlyBall = FlyBall {flyball :: Actor ActorData, vx :: Float, vy :: Float}
  deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

data GSActors = GSActors
  { balls :: M.Map NameId (Actor ActorData)
  , flyingBall :: Maybe FlyBall
  , gun :: Actor ActorData
  , dragon :: Actor ActorData
  , ballQueue :: Actor ActorData
  } deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

emptyGSActors :: GSActors
emptyGSActors = GSActors {
    balls = M.empty
  , flyingBall = Nothing
  , gun = actorMock (ActorGun gunMock)
  , dragon = actorMock (ActorDragon dragonMock)
  , ballQueue = actorMock (ActorBallQueue ballQueueMock)
}

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
  } deriving (Show)

initialGameState :: GameState
initialGameState = GameState {
    gameStepNumber = 0
  , score = 0
  , canvasWidth = 0
  , canvasHeight = 0
  , ballSpeed = 0
  , gameIsRunning = True
  , shotsCounter = 0
  , lastRowsAdded = emptyGSLastRowsAdded
  , actors = emptyGSActors
  , remoteActors = emptyGSActors
  , graphBall = []
  , boards = GSBoards emptyBoard emptyBoard
}

modgs :: (GameState -> GameState) -> AppMod ()
modgs = modify




class ActorContainer ac where
  getAllActors :: GameState -> [Actor ac]
  updateActor :: NameId -> Maybe String -> (Actor ac -> Actor ac) -> AppMod ()
  lookupActor :: NameId -> Maybe String -> GameState -> Maybe (Actor ac)



instance ActorContainer ActorData where
  getAllActors gameState = 
    let as = actors gameState
        ras = remoteActors gameState
        staticActors = 
          ballQueue as : 
          dragon as : 
          gun as : 
          ballQueue ras : 
          dragon ras : 
          gun ras : 
          (M.elems (balls as) <> M.elems (balls ras))
     in map flyball (catMaybes [flyingBall as, flyingBall ras]) <> staticActors

  updateActor nId mbTypeName f = do
    g <- get
    case mbTypeName of
      Nothing -> updateActorAnyType g
      Just "ActorGun" -> updateActorGun 
      Just "ActorBall" -> updateActorBall 
      Just "ActorDragon" -> updateActorDragon 
      Just "ActorBallQueue" -> updeteActorBallQueue
      Just "RemoteActorGun" -> updateRemoteActorGun 
      Just "RemoteActorBall" -> updateRemoteActorBall 
      Just "RemoteActorDragon" -> updateRemoteActorDragon 
      Just "RemoteActorBallQueue" -> updeteRemoteActorBallQueue 
      _ -> updateActorAnyType g
    where
      updateActorGun = do
        modgs $ \gs@GameState{actors = ac@GSActors{gun}} -> gs {actors = ac {gun = f gun}}
      updateRemoteActorGun = do
        modgs $ \gs@GameState{remoteActors = rac@GSActors{gun}} -> gs {remoteActors = rac {gun = f gun}}
      updateActorBall = do
            modgs $ \gs@GameState{actors = ac@GSActors{flyingBall, balls}} ->
                case flyingBall of
                  Just fb@FlyBall{flyball} | nameId flyball == nId ->
                        gs {actors = ac {flyingBall = Just fb{flyball = f flyball}}}
                  _ -> let newBalls = M.update (Just . f) nId balls
                        in gs {actors = ac { balls = newBalls} }
      updateRemoteActorBall = do
        modgs $ \gs@GameState{remoteActors = rac@GSActors{flyingBall, balls}} ->
                case flyingBall of
                  Just fb@FlyBall{flyball} | nameId flyball == nId ->
                        gs {remoteActors = rac {flyingBall = Just fb{flyball = f flyball}}}
                  _ -> let newBalls = M.update (Just . f) nId balls
                        in gs {remoteActors = rac { balls = newBalls} }

      updateActorDragon = do
        modgs $ \gs@GameState{actors = ac@GSActors{dragon}} -> gs {actors = ac {gun = f dragon} }
      updateRemoteActorDragon = do
        modgs $ \gs@GameState{remoteActors = rac@GSActors{dragon}} -> gs {remoteActors = rac {gun = f dragon} }
      updeteActorBallQueue = do
        modgs $ \gs@GameState{actors = ac@GSActors{ballQueue}} -> gs {actors = ac {gun = f ballQueue} }
      updeteRemoteActorBallQueue = do
        modgs $ \gs@GameState{remoteActors = rac@GSActors{ballQueue}} -> gs {remoteActors = rac {gun = f ballQueue} }
      updateActorAnyType g
        | checkActorNameId nId (gun . actors $ g) = updateActorGun 
        | checkActorNameId nId (dragon . actors $ g) = updateActorDragon 
        | checkActorNameId nId (ballQueue . actors $ g) = updeteActorBallQueue
        | checkActorNameId nId (gun . remoteActors $ g) = updateRemoteActorGun 
        | checkActorNameId nId (dragon . remoteActors $ g) = updateRemoteActorDragon 
        | checkActorNameId nId (ballQueue . remoteActors $ g) = updeteRemoteActorBallQueue
        | otherwise = do
            updateActorBall
            updateRemoteActorBall 

  lookupActor nId mbTypeName model =
    let ga = actors model
        gar = remoteActors model
    in
      case mbTypeName of
        Nothing -> lookupActorAnyType ga 
        Just "ActorGun" -> lookupGun ga
        Just "ActorBall" -> lookupBall ga
        Just "ActorDragon" -> lookupDragon ga
        Just "ActorBallQueue" -> lookupBallQueue ga
        Just "RemoteActorGun" -> lookupGun gar
        Just "RemoteActorBall" -> lookupBall gar
        Just "RemoteActorDragon" -> lookupDragon gar
        Just "RemoteActorBallQueue" -> lookupBallQueue gar
        _ -> lookupActorAnyType ga
    where
      lookupGun ga       = if checkActorNameId nId (gun ga) then Just (gun ga) else Nothing
      lookupBall ga      = case flyingBall ga of
                              Just FlyBall{flyball} | nameId flyball == nId -> Just flyball
                              _ -> M.lookup nId (balls ga)
      lookupDragon ga    = if checkActorNameId nId (dragon ga) then Just (dragon ga) else Nothing
      lookupBallQueue ga = if checkActorNameId nId (ballQueue ga) then Just (ballQueue ga) else Nothing 
      lookupActorAnyType ga
        | checkActorNameId nId (gun ga) = Just (gun ga)
        | checkActorNameId nId (dragon ga) = Just (dragon ga)
        | checkActorNameId nId (ballQueue ga) = Just (ballQueue ga)
        | otherwise = M.lookup nId (balls ga)
