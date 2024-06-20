module PriorityQueue (
    PriorityQueue,
    empty,
    insert,
    extractMin,
    decreaseKey
) where

import Data.Maybe (fromJust, isJust)

data BinomialTree k v = Node Int k v [BinomialTree k v] deriving (Show)

newtype BinomialHeap k v = BinomialHeap [BinomialTree k v] deriving (Show)

type PriorityQueue k v = BinomialHeap k v

empty :: PriorityQueue k v
empty = BinomialHeap []

rank :: BinomialTree k v -> Int
rank (Node r _ _ _) = r

root :: BinomialTree k v -> k
root (Node _ k _ _) = k

link :: Ord k => BinomialTree k v -> BinomialTree k v -> BinomialTree k v
link t1@(Node r1 k1 v1 c1) t2@(Node _ k2 v2 c2)
    | k1 <= k2  = Node (r1 + 1) k1 v1 (t2 : c1)
    | otherwise = Node (r1 + 1) k2 v2 (t1 : c2)

insTree :: Ord k => BinomialTree k v -> BinomialHeap k v -> BinomialHeap k v
insTree t (BinomialHeap ts) = BinomialHeap (ins ts)
  where
    ins [] = [t]
    ins ts@(t':ts')
        | rank t < rank t' = t : ts
        | otherwise        = let BinomialHeap rest = insTree (link t t') (BinomialHeap ts') in rest

insert :: Ord k => k -> v -> PriorityQueue k v -> PriorityQueue k v
insert k v = insTree (Node 0 k v [])

merge :: Ord k => BinomialHeap k v -> BinomialHeap k v -> BinomialHeap k v
merge (BinomialHeap h1) (BinomialHeap h2) = BinomialHeap (merge' h1 h2)
  where
    merge' [] h = h
    merge' h [] = h
    merge' ts1@(t1:ts1') ts2@(t2:ts2')
        | rank t1 < rank t2 = t1 : merge' ts1' ts2
        | rank t2 < rank t1 = t2 : merge' ts1 ts2'
        | otherwise         = let BinomialHeap rest = insTree (link t1 t2) (BinomialHeap (merge' ts1' ts2')) in rest

removeMinTree :: Ord k => [BinomialTree k v] -> (BinomialTree k v, [BinomialTree k v])
removeMinTree [] = error "empty heap"
removeMinTree [t] = (t, [])
removeMinTree (t:ts)
    | root t <= root t' = (t, ts)
    | otherwise         = (t', t:ts')
  where
    (t', ts') = removeMinTree ts

extractMin :: Ord k => PriorityQueue k v -> Maybe ((k, v), PriorityQueue k v)
extractMin (BinomialHeap []) = Nothing
extractMin (BinomialHeap ts) = Just ((k, v), BinomialHeap (merge' (reverse ts1) ts2))
  where
    (Node _ k v ts1, ts2) = removeMinTree ts
    merge' [] h = h
    merge' h [] = h
    merge' ts1@(t1:ts1') ts2@(t2:ts2')
        | rank t1 < rank t2 = t1 : merge' ts1' ts2
        | rank t2 < rank t1 = t2 : merge' ts1 ts2'
        | otherwise         = let BinomialHeap rest = insTree (link t1 t2) (BinomialHeap (merge' ts1' ts2')) in rest

decreaseKey :: Ord k => k -> k -> PriorityQueue k v -> PriorityQueue k v
decreaseKey oldKey newKey (BinomialHeap ts) = BinomialHeap (map decrease ts)
  where
    decrease t@(Node r k v c)
        | k == oldKey = Node r newKey v c
        | otherwise   = Node r k v (map decrease c)
