module Chap05Queue where

import Test.QuickCheck
import Test.QuickCheck.Arbitrary

import Queue


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



data DoubleEndQueue a = DQ [a] [a] deriving Show

checkfDQ :: DoubleEndQueue a -> DoubleEndQueue a
checkfDQ dq@(DQ f r)
  | null f = DQ (reverse $ take halfR r) (drop halfR r)
  | null r = DQ (drop halfF f) (reverse $ take halfF f)
  | otherwise = dq
  where halfR = length r `div` 2
        halfF = length f `div` 2

instance Queue DoubleEndQueue where
  empty = DQ [] []

  isEmpty (DQ [] []) = True
  isEmpty (DQ _ _) = False

  snoc (DQ f r) a = checkfDQ $ (DQ f (a:r))

  headq dq@(DQ f r)
    | isEmpty dq = error "empty deque"
    | null f = last r
    | null r = head f
    | otherwise = head f

  tailq dq@(DQ f r)
    | isEmpty dq = error "empty deque"
    | null f = checkfDQ $ DQ f (init r)
    | null r = checkfDQ $ DQ (tail f) r
    | otherwise = checkfDQ $ DQ (tail f) r

instance Deque DoubleEndQueue where
  cons a dq@(DQ f r) = checkfDQ $ DQ (a:f) r

  lastq dq@(DQ f r)
    | isEmpty dq = error "empty deque"
    | null f = head r
    | null r = last f
    | otherwise = last f

  initq dq@(DQ f r)
    | isEmpty dq = error "empty deque"
    | null f = checkfDQ $ DQ f (tail r)
    | null r = checkfDQ $ DQ (init f) r
    | otherwise = checkfDQ $ DQ (init f) r

-- TODO: exercise 5.2 prove the bigO is O(1) amortized



queueTest :: IO ()
queueTest = do
  quickCheck (prop_head_tail :: BatchedQueue Int -> Bool)
