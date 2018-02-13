module SizedLazyBinomialHeap where

import Heap
import qualified Chap03BinomialHeap as BH

data SizedBHeap a = SBHeap (BH.BHeap a)

instance Heap SizedBHeap where
  empty = SBHeap 0 empty
  isEmpty (SBHeap size bh) = size == 0

  insert a sbt@(SBHeap size bh) = SBHeap size' (insert a bh)
    where !size' = size + 1

  merge (SBHeap s1 bh1) (SBHeap s2 bh2) = SBHeap total (merge bh1 bh2)
    where !total = s1 + s2

  findMin (SBHeap size bh) = findMin bh

  deleteMin sbh@(SBHeap size bh)
    | isEmpty sbh = error "empty heap"
    | otherwise = SBHeap size' (deleteMin bh)
    where !size' = size - 1
