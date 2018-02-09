module Chap02Stack where

import Test.QuickCheck
import Test.QuickCheck.Arbitrary

import Stack

-- Stack implement with the built-in list
newtype List a = L [a] deriving Show

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    return $ L a

instance Stack List where
  none = L []

  isEmpty (L []) = True
  isEmpty _ = False

  peek (L []) = error "No element"
  peek (L (a:as)) = a

  pop (L []) = error "No element"
  pop (L (a:as)) = (a, L as)

  push x (L as) = L $ x:as
  len (L as) = length as

prop_add a ls = len ls + 1 == len (push a ls)

stackTest :: IO ()
stackTest = do
  quickCheck (prop_add :: Int -> List Int -> Bool)
