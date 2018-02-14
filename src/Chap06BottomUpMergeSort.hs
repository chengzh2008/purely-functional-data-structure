module Chap06BottomUpMergeSort where

import Sortable

{-
Bottom-up mergesort first splits a list into n ordered segments (signleton segment). It then merges equal-sized segments in pairs until only one segment of each sie remains. Finally segments of unequal size are merged from smallest to largest.
-}

{-
The size of all segments are distinct powers of 2, corresponding to the one bits of size n. The individual segments are stored in increasing order of size, and the elements in each segment are stored in increasing order.
-}
