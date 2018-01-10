module Chap03RedblackTree where

import Test.QuickCheck
import Control.Monad

-- Set interface
class Set s where
  empty :: s a
  insert :: Ord a => a -> s a -> s a
  member :: Ord a => a -> s a -> Bool

data Color = R | B deriving (Show, Eq)
-- all empty node are considered black, thus no color is needed for E constructor
data RBTree a = E | T Color (RBTree a) a (RBTree a) deriving (Show)

{-
  Two invariants for red-black tree:
  1. No red node has red child
  2. Every path from the root to an empty node contains the same number of black nodes.

  Together, these two invariants guarantee that the longest possible path in a red-black tree, one with alternating black and red nodes, is no more than twice as long as the shortest possible path, one with black nodes only.
-}

-- Exercise 3.8 Prove that the maximum depth of a node in a red-black tree of size n is at most 2* floor(log(n + 1)).

color :: RBTree a -> Color
color E = B
color (T c _ _ _) = c

instance Set RBTree where
  empty = E

  member x E = False
  member x (T c l y r)
    | x == y = True
    | x < y = member x l
    | otherwise = member x r

  insert x rbt = T B l y r
    where (T _ l y r) = ins rbt
          ins E = T R E x E
          ins t@(T c l y r)
            | x == y = t
            | x < y = balance c (ins l) y r
            | otherwise = balance c l y (ins r)

balance :: Color -> RBTree a -> a -> RBTree a -> RBTree a
balance B (T R ((T R a x b)) y c) z d = T R (T B a x b) y (T B c z d)
balance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
balance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
balance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
balance c a x b  = T c a x b

instance (Arbitrary a, Ord a) => Arbitrary (RBTree a) where
  arbitrary = sized rbtree'
    where rbtree' 0 = do
            return $ E
          rbtree' n = do
            frequency [ (1, rbtree' 0)
                      , (100, do
                            a <- arbitrary
                            rbt <- rbtree' (n-1)
                            return $ insert a rbt
                        )]

prop_color rbt = go rbt
  where go :: RBTree a -> Bool
        go E = True
        go (T c l a r)
          | c == R = R /= cl && R /= cr && gorest
          | otherwise = gorest
          where cl = color l
                cr = color r
                gorest = go l && go r

-- see some example of LHeap generated randomly
test :: IO ()
test = do
  sample (arbitrary :: Gen (RBTree Int))


rbtreeTest :: IO ()
rbtreeTest = do
  quickCheck (prop_color :: RBTree Int -> Bool)
