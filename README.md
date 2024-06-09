# Dijkstra's Algorithm in Haskell

This project implements Dijkstra's algorithm in Haskell to find the shortest path between nodes in a weighted graph. The graph is represented as an adjacency list and read from an input file (`graph.txt`). The algorithm uses a priority queue implemented with a binomial heap. The shortest path results are output in Graphviz format to `output.txt` for visualization.

## How to Run

1. Ensure `graph.txt` contains the graph data in the following format:
```
node1 node2 weight
node1 node3 weight
```

2. Load and run the project in GHCi:
```bash
ghci
:l Graph.hs
:l PriorityQueue.hs
:l Dijkstra.hs
:l main.hs
runMain
```

Requirements:
Haskell Platform, Graphviz