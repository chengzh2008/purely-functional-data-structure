module SplayHeap where


-- splay tree maintains no explicit balance information. Every operation blindly restructures the tree using some simple transformations that tend to increase balance. worst case operation can take as much as O(n), while every operation runs in O(logn) amortized time


data Tree a = E | T (Tree a) a (Tree a) deriving Show

-- insert: partition the tree into two parts, construce a new node at the root of the tree.
insert :: Ord a => a -> Tree a -> Tree a
insert a t = T (smaller a t) a (bigger a t)

-- get bigger partition of the tree
bigger :: Ord a => a -> Tree a -> Tree a
bigger a E = E
bigger a (T l x r) =
  if x <= a then bigger a r
  else T (bigger a l) x r

-- get smaller partition of the tree
smaller :: Ord a => a -> Tree a -> Tree a
smaller a E = E
smaller a (T l x r) =
  if x <= a then T l x (smaller a r)
  else smaller a l


-- restructuring tree structure heuristic: every time we follow two left branches (or two right branches) in a row, we rotate those two nodes
{-
                 x                            y
        a -->  /   \     right rotate       /   \
             y     r         ===>         l'     x
    a -->  /   \                               /   \
          l'    r'                            r'    r
-}
bigger' :: Ord a => a -> Tree a -> Tree a
bigger' a E = E
bigger' a (T l x r) =
  if x <= a then bigger' a r
  else case l of
     E ->  T E x r
     T l' y r' ->
       if  y <= a then T (bigger' a r') x r
       else T (bigger' a l') y (T r' x r)

-- rotate for smaller as well
{-
                y                            x
              /   \      left rotate       /   \    <-- a
             x     r'        <===         l     y
           /   \                               /   \   <-- a
          l    l'                            l'    r'
-}
smaller' :: Ord a => a -> Tree a -> Tree a
smaller' a E = E
smaller' a (T l x r) =
  if x <= a then case r of
    E -> T l x E
    T l' y r' ->
      if y <= a then T (T l x l') y (smaller' a r')
      else T (T l x l') y (smaller' a r')
  else smaller' a l


-- combine two functions together
partition :: Ord a => a -> Tree a -> (Tree a, Tree a)
partition a E = (E, E)
partition a (T l x r) = undefined
