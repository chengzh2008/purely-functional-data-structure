module Queue where

-- Queue interface
class Queue q where
  empty :: q a
  isEmpty :: q a -> Bool
  snoc :: q a -> a -> q a
  headq :: q a -> a
  tailq :: q a -> q a

-- Deque class
-- exercise 5.1 implementation of double deque
class Queue q => Deque q where
  cons :: a -> q a -> q a
  lastq :: q a -> a
  initq :: q a -> q a
