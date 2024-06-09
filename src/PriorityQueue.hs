module PriorityQueue (PriorityQueue, empty, insert, extractMin) where

import qualified Data.Map as Map

data PriorityQueue k v = PriorityQueue (Map.Map k v)
  deriving (Show)

empty :: PriorityQueue k v
empty = PriorityQueue Map.empty

insert :: (Ord k) => k -> v -> PriorityQueue k v -> PriorityQueue k v
insert key value (PriorityQueue pq) = PriorityQueue (Map.insert key value pq)

extractMin :: (Ord k) => PriorityQueue k v -> Maybe ((k, v), PriorityQueue k v)
extractMin (PriorityQueue pq) = case Map.minViewWithKey pq of
  Just ((k, v), pq') -> Just ((k, v), PriorityQueue pq')
  Nothing -> Nothing
