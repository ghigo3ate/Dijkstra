module Main where

import System.Environment (getArgs)
import Graph
import Dijkstra (dijkstra, Distance, Predecessor)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import System.IO (writeFile)

parseGraph :: String -> Graph
parseGraph = createGraph . map parseLine . lines
  where
    parseLine :: String -> Edge
    parseLine line =
      let [u, v, w] = words line
      in (u, v, read w)

generateGraphviz :: Graph -> Map.Map Node Distance -> Map.Map Node Predecessor -> Node -> Node -> String
generateGraphviz graph distances predecessors start end =
  "digraph G {\n" ++
  concatMap (\(u, vs) -> concatMap (\(v, w) -> "  " ++ u ++ " -> " ++ v ++ " [label=\"" ++ show w ++ "\"];\n") vs) (Map.toList graph) ++
  "  " ++ start ++ " [color=blue];\n" ++
  "  " ++ end ++ " [color=red];\n" ++
  concatMap (\(v, Just u) -> "  " ++ u ++ " -> " ++ v ++ " [color=green];\n") (Map.toList predecessors) ++
  "}\n"

outputResults :: Graph -> Map.Map Node Distance -> Map.Map Node Predecessor -> Node -> Node -> IO ()
outputResults graph distances predecessors start end = do
  let dist = fromMaybe (error "No path found") (Map.lookup end distances)
  putStrLn $ "Shortest path from " ++ start ++ " to " ++ end ++ " is " ++ show dist
  let graphviz = generateGraphviz graph distances predecessors start end
  writeFile "output.txt" graphviz
  putStrLn "Graphviz output written to output.txt"

main :: IO ()
main = do
  args <- getArgs
  let [fileName, start, end] = args
  content <- readFile fileName
  let graph = parseGraph content
      (distances, predecessors) = dijkstra graph start
  outputResults graph distances predecessors start end

-- for testing
runMain :: IO ()
runMain = do
  let fileName = "graph.txt"
      start = "1"
      end = "5"
  content <- readFile fileName
  let graph = parseGraph content
      (distances, predecessors) = dijkstra graph start
  outputResults graph distances predecessors start end
