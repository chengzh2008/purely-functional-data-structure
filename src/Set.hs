module Set where

-- Set interface
class Set s where
  empty :: s a
  insert :: Ord a => a -> s a -> s a
  member :: Ord a => a -> s a -> Bool
