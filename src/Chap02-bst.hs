module Chap02BSTSet where


-- Set interface
class Set s where
  empty :: s a
  insert :: (Ord a) => a -> s a -> s a
  member :: (Ord a) => a -> s a -> Bool

--Binary Search Tree
data BSTSet a = E | T (BSTSet a) a (BSTSet a) deriving (Show)

instance Set BSTSet where
  empty = E

  insert x E = T E x E
  insert x bst@(T l y r)
    | x < y = T (insert x l) y r
    | x > y = T l y (insert x r)

  member x E = False
  member x bst@(T l y r)
    | x < y = member x l
    | x > y = member x r
    | otherwise = True


-- exercise 2.2
-- implement member with no more then d + 1 comparison described in the paper: Arne Andersson. A note on searching in a binary search tree. Software—Practice and Experience, 21 (10): 1125–1128, October 1991.

newtype LCBSTSet a = LC (BSTSet a)

instance Set LCBSTSet where
  empty = LC empty

  insert x (LC bst) = LC $ insert x bst

  member x (LC bst) = go x Nothing bst
    -- carry a candidate around, when reach an empty node, then compare search query with the candidate, which results in at most d + 1 comparison.
    where go :: (Ord a) => a -> Maybe a -> BSTSet a -> Bool
          go _ Nothing E = False
          go m (Just c) E = m == c
          go m cand (T l n r)
            | m < n = go m cand l
            | otherwise = go m (Just n) r

