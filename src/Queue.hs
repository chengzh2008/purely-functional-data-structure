module Queue where

-- Queue interface
class Queue q where
  empty :: q a
  isEmpty :: q a -> Bool
  snoc :: q a -> a -> q a
  headq :: q a -> a
  tailq :: q a -> q a
