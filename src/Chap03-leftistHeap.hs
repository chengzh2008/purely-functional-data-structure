{-# LANGUAGE FlexibleContexts #-}
module Chap03LeftistHeap where

import Test.QuickCheck
import Control.Monad

import Heap


type Rank = Integer
data LHeap a = E | T Rank a (LHeap a) (LHeap a) deriving (Show, Eq, Ord)

-- arbitrary implementation for a sized random tree generation
instance (Arbitrary a, Ord a) =>  Arbitrary (LHeap a) where
  arbitrary = sized lheap'
    where lheap' 0 = do
            a <- arbitrary
            return $ T 1 a E E
          lheap' n = do
            oneof [ lheap' 0
                  , do
                      a <- arbitrary
                      lh <- lheap' (n - 1)
                      return $ insert a lh
                  ]
            where subHeap = lheap' $ n - 1

rank :: LHeap a -> Rank
rank E = 0
rank (T r _ _ _) = r

-- Exercise 3.1 Prove that the right spine of a leftist heap of size n contains at most [log(n + 1)] elements. (All logarithms in this book are base 2 unless otherwise indicated.)

instance Heap LHeap where
  empty = E

  isEmpty E = True
  isEmpty _ = False

  insert x h = merge (T 1 x E E) h

  merge E h = h
  merge h E = h
  merge h1@(T _ a1 l1 r1) h2@(T _ a2 l2 r2)
    | a1 <= a2 = makeT a1 l1 $ merge r1 h2
    | otherwise = makeT a2 l2 $ merge h1 r2
    where makeT :: a -> LHeap a -> LHeap a -> LHeap a
          makeT a l r
            | rank l >= rank r = T (rank r + 1) a l r
            | otherwise = T (rank l + 1) a r l

  findMin E = error "empty heap"
  findMin (T rank a l r) = a

  deleteMin E = error "empty heap"
  deleteMin (T rank a l r) = merge l r

-- Exercise 3.2 Define insert directly rather than via a call to merge.
-- TODO:


-- Exercise 3.3 Implement a function fromList of type Elem.T list → Heap that produces a leftist heap from an unordered list of elements by first converting each element into a singleton heap and then merging the heaps until only one heap remains. Instead of merging the heaps in one right-to-left or left-to-right pass using foldr or foldl, merge the heaps in [log n] passes, where each pass merges adjacent pairs of heaps. Show that fromList takes only O(n) time.

lheapFromList :: Ord a => [a] -> LHeap a
lheapFromList as = head $ go sig
  where sig = map (\a -> T 1 a E E) as
        go :: Ord a => [LHeap a] -> [LHeap a]
        go [] = []
        go [lh] = [lh]
        go xs = helper xs_l xs_r
          where (xs_l, xs_r) = splitAt (length xs `div` 2) xs
                helper [] r = r
                helper l [] = l
                helper (l:ls) (r:rs) = go next
                  where next = (merge l r) : (helper ls rs)


-- rank properties
prop_rank lh = case lh of
  E -> True
  (T r a leftH rightH) -> r == rank rightH + 1 -- root rank is the rank of right subtree + 1
                       && rank leftH >= rank rightH -- left rank is at least large as the right one
                       && prop_rank leftH -- its left tree comply with the rank properties
                       && prop_rank rightH -- its right tree comply with the rank properties
-- order properties
prop_order lh = case lh of
  E -> True
  (T r a leftH rightH) -> orderP a leftH
                       && orderP a rightH -- root is smaller than both its sub tree node
                       && prop_order leftH -- left subtree order properties
                       && prop_order rightH -- right subtree order properties
    where orderP :: Ord a => a -> LHeap a -> Bool
          orderP _ E = True
          orderP a (T _ a1 l r) = a <= a1

-- properties for lheapFromList: minimum of the list is the same of findMin from the converted LHeap
prop_order_fromList xs = case xs of
  [] -> True -- do not test empty list
  _ -> findMin (lheapFromList xs) == minimum xs

leftistHeapTest :: IO ()
leftistHeapTest= do
  quickCheck (prop_rank :: LHeap String -> Bool)
  quickCheck (prop_order :: LHeap String -> Bool)
  quickCheck (prop_order_fromList:: [String]-> Bool)


-- see some example of LHeap generated randomly
test :: IO ()
test = do
  sample (arbitrary :: Gen (LHeap Int))
