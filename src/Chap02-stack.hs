module Chap02Stack where

import Test.QuickCheck (quickCheck)

-- Stack interface

class Stack st where
  none :: st a
  isEmpty :: st a -> Bool
  peek :: st a -> a
  pop :: st a -> (a, st a)
  push :: a -> st a -> st a
  length :: st a -> Int


-- Stack implement with the built-in list
newtype List a = L [a]

instance Stack List where
  none = None

  isEmpty None = True
  isEmpty _ = False

  peek None = error "No element"
  peek (More a _) = a

  pop None = error "No element"
  pop (More a la) = (a, la)

  push x None = More x None
  push x xs = More x xs


stackTest :: IO ()
stackTest = do
  quickCheck (prop_empty :: [Bool] -> Bool)
