module Chap06LazyParingHeap where

import Heap


data LazyParingHeap a =
  E | LPH a (LazyParingHeap a) (LazyParingHeap a) deriving Show


merge' :: Ord a => LazyParingHeap a -> LazyParingHeap a -> LazyParingHeap a
merge' a E = a
merge' E a = a
merge' lph1@(LPH a1 _ _) lph2@(LPH a2 _ _)
  | a1 <= a2 = link lph1 lph2
  | otherwise = link lph2 lph1


link :: Ord a => LazyParingHeap a -> LazyParingHeap a -> LazyParingHeap a
link (LPH x E lph1) lph = LPH x lph lph1
link (LPH x lph1 lph) lph2 = LPH x E (merge' (merge' lph1 lph2) lph)

instance Heap LazyParingHeap where
  empty = E
  isEmpty E = True
  isEmpty _ = False

  merge = merge'

  insert x lph = merge (LPH x E E) lph

  findMin E = error "empty heap"
  findMin (LPH x _ _) = x

  deleteMin E = error "empty heap"
  deleteMin (LPH _ lph1 lph2) = merge lph1 lph2

-- a lot of similar to tree-based implementation
