{-# LANGUAGE FlexibleContexts #-}
module Chap03BinomialHeap where

import Test.QuickCheck
import Control.Monad
import Data.List (minimumBy)

import Heap


type Rank = Integer

-- list of children is maintenanced in decreasing order of rank
data BTree a = BTree { rank :: Int
                     , item :: a
                     , children :: [BTree a]
                     } deriving (Show)

-- link two trees into one tree, always link trees with equal rank
-- keep heap order by always linking trees with larger roots under trees with smaller roots.
link :: Ord a => BTree a -> BTree a -> BTree a
link bh1@(BTree r a1 c1) bh2@(BTree _ a2 c2)
  | a1 <= a2 = BTree (r + 1) a1 (bh2:c1)
  | otherwise = BTree (r + 1) a2 (bh1:c2)

-- a binomial heap is a list of binomial trees in increasing order of rank and no tree has the same rank
data BHeap a = BHeap { ls :: [BTree a]
                     } deriving Show

instance (Arbitrary a) => Arbitrary (BTree a) where
  arbitrary = do
    a <- arbitrary
    return $ BTree 0 a []

instance (Arbitrary a, Ord a) => Arbitrary (BHeap a) where
  arbitrary = sized bheap'
    where bheap' 0 = do
            return $ BHeap []
          bheap' n = do
            -- using frequency to produce more samples with nom-empty binomial heap
            frequency [ (1, bheap' 0)
                  , (5, do
                      a <- arbitrary
                      bh <- bheap' (n - 1)
                      return $ insert a bh
                    )
                  ]

insTree :: Ord a => BTree a -> BHeap a -> BHeap a
insTree bt bh = case ls bh of
  [] -> BHeap [bt]
  bt2@(t:ts) -> if rank bt < rank t
                then BHeap (bt:bt2)
                else insTree (link bt t) (BHeap ts)

removeMinTree :: Ord a => [BTree a] -> (BTree a, [BTree a])
removeMinTree [t] = (t, [])
removeMinTree (bt:bts) =
  let (t', ts') = removeMinTree bts
  in if item bt < item t' then (bt, reverse bts) else (t', reverse (bt:ts'))

instance Heap BHeap where
  empty = BHeap []

  isEmpty bh = case ls bh of
    [] -> True
    _ -> False

  insert a bt = insTree (BTree 0 a []) bt

  merge (BHeap []) bh2 = bh2
  merge bh1 (BHeap []) = bh1
  merge (BHeap x@(bt1:bt1s)) (BHeap y@(bt2:bt2s))
    | rank bt1 < rank bt2 = BHeap $ bt1 : ls (merge (BHeap bt1s) (BHeap  y))
    | rank bt1 > rank bt2 = BHeap $ bt2 : ls (merge (BHeap x) (BHeap bt2s))
    | otherwise = insTree (link bt1 bt2) $ merge (BHeap bt1s) (BHeap bt2s)

  findMin (BHeap []) = error "empty heap"
  findMin (BHeap bts) = item $ fst $ removeMinTree bts
  --findMin (BHeap bts) = item $ minimumBy (\bt1 bt2 -> compare (item bt1) (item bt2)) bts

  deleteMin (BHeap []) = error "empty heap"
  deleteMin (BHeap bts) =
    let (bt, bts') = removeMinTree bts
    in merge (BHeap (reverse $ children bt)) (BHeap bts')

-- Exercise 3.5 Define findMin directly rather than via a call to removeMinTree.
-- TODO:


-- see some example of LHeap generated randomly
test :: IO ()
test = do
  sample (arbitrary :: Gen (BHeap Int))

-- binomial heap properties
-- rank is in increasing order for the tree list
prop_rank_order (BHeap []) = True
prop_rank_order (BHeap [bt]) = True
prop_rank_order (BHeap (bt1:bt2:bts)) =
  rank bt1 <= rank bt2
  && prop_rank_order (BHeap (bt2:bts))

-- rank of trees in the children list is in decreasing order
childrenRankOrder :: [BTree a] -> Bool
childrenRankOrder [] = True
childrenRankOrder [x] = True
childrenRankOrder (x:y:zs) = rank x > rank y && childrenRankOrder (y:zs)

-- for each BTree a, item is smaller than all items in its children
treeItemOrder :: Ord a => a -> [BTree a] -> Bool
treeItemOrder _ [] = True
treeItemOrder a (bt:bts) = a <= item bt && treeOrder bt && treeItemOrder a bts

treeOrder :: Ord a => BTree a -> Bool
treeOrder (BTree rank a []) = rank == 0
treeOrder (BTree rank a cs@(t:ts)) = treeItemOrder a cs && childrenRankOrder cs


prop_heap_order (BHeap []) = True
prop_heap_order (BHeap (bt:bts)) = treeOrder bt
                                && prop_heap_order (BHeap bts)

binomialHeapTest :: IO ()
binomialHeapTest = do
  quickCheck (prop_rank_order:: BHeap String -> Bool)
  quickCheck (prop_heap_order:: BHeap String -> Bool)



-- Exercise 3.6 Most of the rank annotations in this representation of binomial heaps are redundant because we know that the children of a node of rank r have ranks r – 1,…, 0. Thus, we can remove the rank annotations from each node and instead pair each tree at the top-level with its rank


-- Exercise 3.7 One clear advantage of leftist heaps over binomial heaps is that findMin takes only O(1) time, rather than O(log n) time. The following functor skeleton improves the running time of findMin to O(1) by storing the minimum element separately from the rest of the heap.
