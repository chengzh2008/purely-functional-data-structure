module Stack where

-- Stack interface

class Stack st where
  none :: st a
  isEmpty :: st a -> Bool
  peek :: st a -> a
  pop :: st a -> (a, st a)
  push :: a -> st a -> st a
  len :: st a -> Int
