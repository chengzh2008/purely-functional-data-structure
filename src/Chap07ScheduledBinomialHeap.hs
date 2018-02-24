module Chap07ScheduledBinomialHeap where

import Heap

data Tree a = Tree a [Tree a] deriving Show
data Digit a = Zero | One (Tree a) deriving Show
data SBHeap a = SBHeap [Digit a] deriving Show
