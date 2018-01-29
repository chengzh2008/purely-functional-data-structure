module Chap05Queue where

import Test.QuickCheck
import Test.QuickCheck.Arbitrary

-- Queue interface
class Queue q where
  empty :: q a
  isEmpty :: q a -> Bool
  snoc :: q a -> a -> q a
  headq :: q a -> a
  tailq :: q a -> q a

-- Queue implementation with two lists
data BatchedQueue a = BQ { front :: [a]
                         , rear :: [a]
                         } deriving Show

instance Arbitrary a => Arbitrary (BatchedQueue a) where
  arbitrary = do
    front <- arbitrary
    rear <- arbitrary
    return $ BQ front rear

checkf ::  BatchedQueue a -> BatchedQueue a
checkf bq = if isEmpty bq
            then BQ (reverse $ rear bq) []
            else bq

instance Queue BatchedQueue where
  empty = BQ [] []
  isEmpty (BQ f r) = case f of
    [] -> True
    otherwise -> False
  snoc bq a = checkf $ BQ (front bq) (a : rear bq)
  headq bq
    | isEmpty bq = error "empty queue"
    | otherwise = head $ front bq
  tailq bq
    | isEmpty bq = error "empty queue"
    | otherwise = checkf $ BQ (tail $ front bq) (rear bq)


prop_head_tail bq
  | isEmpty bq = True
  | length (front bq) == 1 = True
  | otherwise = let h = headq bq
                    t = tailq bq
                in front bq == h : front t

someBQ :: IO ()
someBQ = do
  sample $ (arbitrary :: Gen (BatchedQueue Int))
  sample $ (arbitrary :: Gen (BatchedQueue String))

queueTest :: IO ()
queueTest = do
  quickCheck (prop_head_tail :: BatchedQueue Int -> Bool)
