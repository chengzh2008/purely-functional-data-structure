module Chap06BottomUpMergeSort where

import Sortable

{-
Bottom-up mergesort first splits a list into n ordered segments (signleton segment). It then merges equal-sized segments in pairs until only one segment of each sie remains. Finally segments of unequal size are merged from smallest to largest.
-}

{-
The size of all segments are distinct powers of 2, corresponding to the one bits of size n. The individual segments are stored in increasing order of size, and the elements in each segment are stored in increasing order.
-}

type Size = Int
data MergeSort a = MS Size [[a]] deriving Show


merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge xs'@(x:xs) ys'@(y:ys)
  | x <= y = x : merge xs ys'
  | otherwise = y : merge xs' ys

mergeSeg :: Ord a => [a] -> [[a]] -> Size -> [[a]]
mergeSeg seg segs size
  | even size = seg : segs
  -- understanding the size `div` 2
  | otherwise = mergeSeg (merge seg $ head segs) (tail segs) (size `div` 2)

instance Sortable MergeSort where
  empty = MS 0 []

  add a ms@(MS size segs) = MS (size + 1) $ mergeSeg [a] segs size

  sort (MS size segs) = foldr (merge) [] segs
