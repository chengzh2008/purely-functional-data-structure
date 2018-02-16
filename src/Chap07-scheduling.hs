{-
  * Virtually all worst-case data structures becomes amortized when implemented in an entirely lazy programming language like haskell.
  * A good hibrid approach: worst-case data structures that use lazy evaluation internally.
  * Implementating the data structure with lazy evaluation and modifying them by force some operation to evaluate thus every operation runs in the allotted time.

  * scheduling is like knocking over a series of dominors starting from the rear, thus whenever one domino falls on another, the other has already beeen knocked over. Then the actual cost of knocking over each domino is small.


-}
