module ParingHeap where



-- heap-ordered multiway trees
data ParingHeap a = E | PH a [ParingHeap a] deriving Show

findMin :: ParingHeap a -> a
findMin ph = go ph
  where go E = error "empty heap"
        go (PH a _) = a


merge :: ParingHeap a -> ParingHeap a -> ParingHeap
merge ph E = ph
merge E ph = ph
merge (PH a as) (PH b bs)
  | a <= b = PH a (as:bs)
  | otherwise = PH b (bs:as)


insert :: a -> ParingHeap a -> ParingHeap a
insert a ph = merge (PH a []) ph

mergePairs [] = E
mergePairs [h] = h
mergePairs (h1:h2:hs) = merge (merge h1 h2) (mergePairs hs)

deleteMin :: ParingHeap a -> ParingHeap a
deleteMin E = error "empty heap"
deleteMin (PH a as) = mergePairs as
