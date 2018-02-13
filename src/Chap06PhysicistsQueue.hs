{-# LANGUAGE BangPatterns #-}
module PhysicistsQueue where

import Queue

type Size = Int
data PhyQueue a = PQ { wc :: [a]
                     , lenF :: Size
                     , front :: [a]
                     , lenR :: Size
                     , rear :: [a]
                     } deriving Show

{-
keep a working copy of the front list for quick head query, keep the size of both front and rear list.
-}

-- check if working copy is empty. If so, make a copy from front list and evaluated it right away
checkw :: PhyQueue a -> PhyQueue a
checkw pq
  | null $ wc pq = let !wc = front pq in pq {wc = front pq}
  | otherwise = pq


-- check if rear is larger than front list. if so, evaluate front and
-- rotate rear to front
check :: PhyQueue a -> PhyQueue a
check pq
  | lenF pq < lenR pq = let !front' = front pq
                            !total = lenF pq + lenR pq
                        in checkw $ PQ front' total (front' ++ reverse (rear pq)) 0 []
  | otherwise = checkw pq

instance Queue PhyQueue where
  empty = PQ [] 0 [] 0 []
  isEmpty = (== 0) . lenF

  snoc (PQ wc lf f lr r) a = check $ PQ wc lf f (lr + 1) (a:r)

  headq pq@(PQ wc _ _ _ _)
    | isEmpty pq = error "empty queue"
    | otherwise = head wc

  tailq pq
    | isEmpty pq = error "empty queue"
    | otherwise = PQ (tail $ wc pq) (lenF pq - 1) (tail $ front pq) (lenR pq) (rear pq)
