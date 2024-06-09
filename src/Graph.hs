module Graph where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)

type Node = String
type Weight = Int
type Edge = (Node, Node, Weight)
type Graph = Map.Map Node [(Node, Weight)]

addEdge :: Graph -> Edge -> Graph
addEdge graph (u, v, w) = Map.insertWith (++) u [(v, w)] graph

createGraph :: [Edge] -> Graph
createGraph = foldl addEdge Map.empty
