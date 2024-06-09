module Dijkstra (dijkstra, Distance, Predecessor) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import PriorityQueue (PriorityQueue, empty, insert, extractMin)
import Graph
import Data.Maybe

type Distance = Int
type Predecessor = Maybe Node

dijkstra :: Graph -> Node -> (Map.Map Node Distance, Map.Map Node Predecessor)
dijkstra graph start = dijkstra' (insert start 0 empty) Map.empty Map.empty
  where
    dijkstra' :: PriorityQueue Node Distance -> Map.Map Node Distance -> Map.Map Node Predecessor -> (Map.Map Node Distance, Map.Map Node Predecessor)
    dijkstra' pq distances predecessors =
      case extractMin pq of
        Nothing -> (distances, predecessors)
        Just ((u, dist), pq') ->
          let neighbors = fromMaybe [] (Map.lookup u graph)
              (pq'', distances', predecessors') = foldl (updateNeighbor u dist) (pq', distances, predecessors) neighbors
          in dijkstra' pq'' distances' predecessors'

    updateNeighbor :: Node -> Distance -> (PriorityQueue Node Distance, Map.Map Node Distance, Map.Map Node Predecessor) -> (Node, Distance) -> (PriorityQueue Node Distance, Map.Map Node Distance, Map.Map Node Predecessor)
    updateNeighbor u dist (pq, distances, predecessors) (v, weight) =
      let alt = dist + weight
      in case Map.lookup v distances of
          Just currentDist | currentDist <= alt -> (pq, distances, predecessors)
          _ -> (insert v alt pq, Map.insert v alt distances, Map.insert v (Just u) predecessors)
