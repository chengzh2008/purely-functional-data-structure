module Chap02BSTSet where

import Test.QuickCheck
import Test.QuickCheck.Arbitrary

import Set

--Binary Search Tree
data BSTSet a = E | T (BSTSet a) a (BSTSet a) deriving (Show)

instance (Arbitrary a) => Arbitrary (BSTSet a) where
  arbitrary = do
    a <- arbitrary
    frequency [(1, return E), (3, return $ T E a E)]

instance Set BSTSet where
  empty = E

  insert x E = T E x E
  insert x bst@(T l y r)
    | x < y = T (insert x l) y r
    | x > y = T l y (insert x r)
    | otherwise = bst

  member x E = False
  member x bst@(T l y r)
    | x < y = member x l
    | x > y = member x r
    | otherwise = True



-- exercise 2.2
-- Rewrite member to take no more than d + 1 comparisons by keeping track of a candidate element that might be equal to the query element (say, the last element for which < returned false or ≤ returned true) and checking for equality only when you hit the bottom of the tree.
-- Arne Andersson. A note on searching in a binary search tree. Software—Practice and Experience, 21 (10): 1125–1128, October 1991.

newtype LessCompBSTSet a = LessComp (BSTSet a) deriving (Show)

instance (Arbitrary a) => Arbitrary (LessCompBSTSet a) where
  arbitrary = do
    a <- arbitrary
    return $ LessComp a

instance Set LessCompBSTSet where
  empty = LessComp empty

  insert x (LessComp bst) = LessComp $ insert x bst

  member x (LessComp bst) = go x Nothing bst
    -- carry a candidate around, when reach an empty node, then compare search query with the candidate, which results in at most d + 1 comparison.
    where go :: (Ord a) => a -> Maybe a -> BSTSet a -> Bool
          go _ Nothing E = False
          go m (Just c) E = m == c
          go m cand (T l n r)
            | m < n = go m cand l
            | otherwise = go m (Just n) r


-- exercise 2.3
-- Inserting an existing element into a binary search tree copies the entire search path even though the copied nodes are indistinguishable from the originals. Rewrite insert using exceptions to avoid this copying. Establish only one handler per insertion rather than one handler per iteration.

newtype LessCopyBSTSet a = LessCopy (BSTSet a) deriving (Show)

instance (Arbitrary a) => Arbitrary (LessCopyBSTSet a) where
  arbitrary = do
    a <- arbitrary
    return $ LessCopy a

instance Set LessCopyBSTSet where
  empty = LessCopy empty

  member x (LessCopy bst) = member x bst

  -- use CPS to avoid unneccessary copying
  insert x (LessCopy bst) =  LessCopy $ go id x bst
    where go f x E = f $ T E x E
          go f x (T l y r)
            | x == y = bst
            | x < y = go (\newL -> f $ T newL y r) x l
            | x > y = go (\newR -> f $ T l y newR) x r


-- exercise 2.4
-- combine 2.2 and 2.3 to create a set implementaiton with less comparison and no copy of the same node through search path

newtype LessCompCopyBSTSet a = LessCompCopy (BSTSet a) deriving (Show)

instance (Arbitrary a) => Arbitrary (LessCompCopyBSTSet a) where
  arbitrary = do
    a <- arbitrary
    return $ LessCompCopy a

instance Set LessCompCopyBSTSet where
  empty = LessCompCopy empty

  member x (LessCompCopy bst) = go x Nothing bst
    -- carry a candidate around, when reach an empty node, then compare search query with the candidate, which results in at most d + 1 comparison.
    where go :: (Ord a) => a -> Maybe a -> BSTSet a -> Bool
          go _ Nothing E = False
          go m (Just c) E = m == c
          go m cand (T l n r)
            | m < n = go m cand l
            | otherwise = go m (Just n) r

  insert x (LessCompCopy bst) = LessCompCopy $ go id x bst
    where go f x E = f $ T E x E
          go f x (T l y r)
            | x == y = bst
            | x < y = go (\newL -> f $ T newL y r) x l
            | x > y = go (\newR -> f $ T l y newR) x r


-- exercise 2.5

-- exercise 2.5 (a)
-- Using this idea, write a function complete of type Elem × int → Tree where complete (x, d) creates a complete binary tree of depth d with x stored in every node. This function should run in O(d) time.
complete :: a -> Int -> BSTSet a
complete x d = go d
  where go 0 = E
        go n = let next = go (n - 1) in T next x next

-- exercise 2.5 (b)
-- Extend this function to create balanced trees of arbitrary size. These trees will not always be complete binary trees, but should be as balanced as possible
balanceN :: a -> Int -> BSTSet a
balanceN x 0 = E
balanceN x n = create2 n
  where create2 0 = E
        create2 m
          | odd m = let k = div (m - 1) 2
                        next = create2 k
                    in T next x next
          | even m = let k = div (m - 1) 2
                         l = create2 k
                         r = create2 $ k + 1
                     in T l x r



-- exercise 2.6 Adapt the UnbalancedSet functor to support finite maps rather than sets.

class FiniteMap m where
  emptyM :: m k a
  bindM :: Ord k => k -> a -> m k a -> m k a
  lookupM :: Ord k => k -> m k a -> Maybe a

data Node k a = N k a deriving Show

instance Eq k => Eq (Node k a) where
  (N k1 _) == (N k2 _) = k1 == k2

instance Ord k => Ord (Node k a) where
  (N k1 _) <= (N k2 _) = k1 <= k2


data BSTMap k a = BM (BSTSet (Node k a)) deriving Show

instance (Arbitrary k, Arbitrary a) => Arbitrary (Node k a) where
  arbitrary = do
    k <- arbitrary
    a <- arbitrary
    return $ N k a

instance (Arbitrary k, Arbitrary a) => Arbitrary (BSTMap k a) where
  arbitrary = do
    bst <- arbitrary
    return $ BM bst

instance FiniteMap BSTMap where
  emptyM = BM empty

  -- k can not be empty string
  bindM k a (BM bst) = BM $ insert (N k a) bst

  lookupM k (BM bst) = go k bst
    where go k E = Nothing
          go k (T l (N k0 v) r)
            | k == k0 = Just v
            | k <= k0 = go k l
            | otherwise = go k r


-- the member returns True for the elem just inserted
prop_add_member a bst = member a (insert a bst) == True

prop_bind_lookup k v bst = lookupM k (bindM k v bst) == Just v

bstSetTest :: IO ()
bstSetTest = do
  quickCheck (prop_add_member :: String -> BSTSet String -> Bool)
  quickCheck (prop_add_member :: String -> LessCompBSTSet String -> Bool)
  quickCheck (prop_add_member :: String -> LessCopyBSTSet String -> Bool)
  quickCheck (prop_add_member :: String -> LessCompCopyBSTSet String -> Bool)
  quickCheck (prop_bind_lookup :: String -> Int -> BSTMap String Int -> Bool)
