module ParingHeap where

import Test.QuickCheck

import Heap

-- heap-ordered multiway trees
data ParingHeap a = E | PH a [ParingHeap a] deriving Show

findMin' :: ParingHeap a -> a
findMin' ph = go ph
  where go E = error "empty heap"
        go (PH a _) = a


merge' :: Ord a => ParingHeap a -> ParingHeap a -> ParingHeap a
merge' ph E = ph
merge' E ph = ph
merge' ph1@(PH a as) ph2@(PH b bs)
  | a <= b = PH a (ph2:as)
  | otherwise = PH b (ph1:bs)


insert' :: Ord a => a -> ParingHeap a -> ParingHeap a
insert' a ph = merge' (PH a []) ph

mergePairs :: Ord a => [ParingHeap a] -> ParingHeap a
mergePairs [] = E
mergePairs [h] = h
mergePairs (h1:h2:hs) = merge' (merge h1 h2) (mergePairs hs)

deleteMin' :: Ord a => ParingHeap a -> ParingHeap a
deleteMin' E = error "empty heap"
deleteMin' (PH a as) = mergePairs as


instance Heap ParingHeap where
  empty = E

  isEmpty E = True
  isEmpty _ = False

  insert = insert'
  findMin = findMin'
  merge = merge'
  deleteMin = deleteMin'


instance (Arbitrary a, Ord a) => Arbitrary (ParingHeap a) where
  arbitrary = do
    as <- arbitrary
    return $ mergePairs $ fmap (\a -> PH a []) as

test :: IO ()
test = do
  sample (arbitrary :: Gen (ParingHeap Int))

-- paring heap properties
prop_heap :: Ord a => ParingHeap a -> Bool
prop_heap t = go t
  where go E = True
                      -- all items in the list are paring heap tree
        go (PH a phs) = all go phs
                      -- the root item is the smallest
                      && all smallerThenA phs
          where smallerThenA (PH b _) = a <= b


paringHeapTest :: IO ()
paringHeapTest = do
  quickCheck (prop_heap :: ParingHeap Int -> Bool)
  quickCheck (prop_heap :: ParingHeap String -> Bool)
