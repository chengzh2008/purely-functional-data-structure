module ParingHeap where


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


class Heap t where
  empty :: t a
  isEmpty :: t a -> Bool
  insert :: Ord a => a -> t a -> t a
  merge :: Ord a => t a -> t a -> t a
  findMin :: t a -> a
  deleteMin :: Ord a => t a -> t a

instance Heap ParingHeap where
  empty = E

  isEmpty E = True
  isEmpty _ = False

  insert = insert'
  findMin = findMin'
  merge = merge'
  deleteMin = deleteMin'
