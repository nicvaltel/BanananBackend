module Domain.GameBot.GraphBall 
  ( GraphNode(..)
  , Graph
  , GraphBall
  , addNodeBall
  , deleteNodeBall
  , findAttachedToCeilingBalls
  , findNotAttachedToCeilingBalls
  ) where

import Reexport
import Domain.GameBot.Actors (Actor(..), BallColor, NameId, ActorData (..), Ball (..))
import qualified Domain.GameBot.Actors as AC
import qualified Data.Map.Strict as Map

data GraphNode a id = Node {nodeId :: id, nodeData :: a, neighbours :: [GraphNode a id]}

instance (Show id, Show a) => Show (GraphNode a id) where
  show (Node{nodeId, nodeData, neighbours})  =
    "Node nodeId = " <> show nodeId <>
    " nodeData = " <> show nodeData <>
    " neighbours = " <> show neighbours

type Graph a id = [GraphNode a id]

getTopLayerNamesId :: Graph a id -> [id]
getTopLayerNamesId = map nodeId

type BallData = BallColor

type GraphBall = Graph BallData NameId

addNodeBall :: Actor ActorData -> [Actor ActorData] -> GraphBall -> GraphBall
addNodeBall Actor{nameId, actorActorData} connectedBalls graph = case actorActorData of
    ActorBall ball | nameId `notElem` getTopLayerNamesId graph -> do -- getTopLayerNamesId is OK because all balls are present at top level of list (no need to recurcsive search)
        let connectedNamesIds = map AC.nameId connectedBalls
        let neighbs = filter (\(Node{nodeId}) -> nodeId `elem` connectedNamesIds) graph
        let newNode = Node 
                { nodeId = nameId
                , nodeData = color ball
                , neighbours = neighbs
                }
        -- attache new ball as a neighbor to old ones
        let updatedGraph = flip map graph $ \node ->
              if nodeId node `elem` connectedNamesIds
                then node{neighbours = newNode : neighbours node}
                else node
        newNode : updatedGraph
    _ -> graph

deleteNodeBall :: NameId -> GraphBall -> GraphBall
deleteNodeBall nameId graph = -- we need to delete ball from top level and from all list of neighbours at top level
  let filteredTopLayer = filter (\(Node{nodeId}) -> nodeId /= nameId) graph
  in flip map filteredTopLayer $ \node -> 
      node{ neighbours = filter (\(Node{nodeId}) -> nodeId /= nameId ) (neighbours node)}

findAttachedToCeilingBalls :: Map.Map NameId (Actor ActorData) -> GraphBall -> [NameId] -> [NameId]
findAttachedToCeilingBalls balls nodesToCheck attachedNodes = do
    let (partY, partN) = flip partition nodesToCheck $ \n -> 
                         isAttacedToCeiling (nodeId n) || 
                         nodeId n `elem` attachedNodes ||
                         any (\m -> nodeId m `elem` attachedNodes) (neighbours n)
    if null partY -- partition.yes for elems, where predicate is true; partition.no for elems where predicate is false
        then attachedNodes
        else findAttachedToCeilingBalls balls partN (attachedNodes <> getTopLayerNamesId partY)
    where
      isAttacedToCeiling :: NameId -> Bool
      isAttacedToCeiling nameId = case Map.lookup nameId balls of
        Nothing -> False
        Just Actor{y} -> y <= 1.0

findNotAttachedToCeilingBalls :: Map.Map NameId (Actor ActorData) -> GraphBall -> [NameId]
findNotAttachedToCeilingBalls balls graph = (\\) (getTopLayerNamesId graph) (findAttachedToCeilingBalls balls graph [])
