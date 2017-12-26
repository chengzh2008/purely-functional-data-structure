module Chap02 where

-- exercise 2.1
suffix :: [a] -> [[a]]
suffix [] = [[]]
suffix a@(x:xs) = a : suffix xs

--Binary Search Tree
data BST a = EmptBST | SomeBST (BST a) a (BST a)
  deriving (Show, Ord, Eq)

empty :: BST a
empty = EmptBST

member :: (Ord a, Eq a) => a -> BST a -> Bool
member x EmptBST = False
member x (SomeBST l a r)
  |  x == a = True
  | x < a = member x l
  | otherwise = member x r

insert :: (Ord a, Eq a) => a -> BST a -> BST a
insert x EmptBST = SomeBST EmptBST x EmptBST
insert x bst@(SomeBST l a r)
  | x == a = bst
  | x < a = SomeBST (insert x l) a r
  | otherwise = SomeBST l a (insert x r)


tree1 = SomeBST EmptBST 3 EmptBST
tree2 = SomeBST tree1 2 tree1
