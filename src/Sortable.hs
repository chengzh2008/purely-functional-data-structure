module Sortable where

class Sortable s where
  empty :: s a
  add :: Ord a => a -> s a -> s a
  sort :: Ord a => s a -> s a
