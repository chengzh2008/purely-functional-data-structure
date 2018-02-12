module BankerQueue where

import Test.QuickCheck
import Test.QuickCheck.Arbitrary

import Queue

type LenF = Int
type LenR = Int

data BankerQueue a = BankerQ LenF [a] LenR [a] deriving Show

{-
periodically rotate the queue by moving all the elements of the rear elements to the end of the front list. reverse is a monolithic funcion. rotate when the |r| > |f| + 1 thereby maintaining the invariant that |f| >= |r|, which inplys that f is empty iff r is empty.

      invariant:  |f| >= |r|
-}

check :: BankerQueue a -> BankerQueue a
check bq@(BankerQ lf f lr r) =
  if lr > lf then BankerQ (lf + lr) (f ++ reverse r) 0 [] else bq

instance Queue BankerQueue where
  isEmpty (BankerQ lf _ _ _) = lf == 0
  empty = BankerQ 0 [] 0 []

  snoc (BankerQ lf f lr r) a = check $ BankerQ lf f (lr + 1) (a:r)

  headq bq@(BankerQ lf f lr r) =
    if isEmpty bq then error "empty queue" else head f

  tailq bq@(BankerQ lf f lr r) =
    if isEmpty bq then error "empty queue" else check $ BankerQ (lf - 1) (tail f) lr r

{-
reasoning:
  1) let q(0) be some queue whose front and rear list rar both of length m
         q(0) = BankerQ m front m rear
  2) let q(i) = tail q(i-1), for 0 < i <= m + 1
  3) the queue is rotated during the first application of the tail, the reverse suspension created by the rotation is forced during th last application of tail.
  4) this reverse takes m steps, and its cost is amortized over the sequence q1 ... qm

-}


instance Arbitrary a => Arbitrary (BankerQueue a) where
  arbitrary = do
    front <- arbitrary
    rear <- arbitrary
    return $ check $ BankerQ (length front) front (length rear) rear


--invariant test
prop_front_rear_length bq@(BankerQ lf f lr r)
  | isEmpty bq = lf == 0 && lr == 0
  | otherwise = lf >= lr

someBankerQ :: IO ()
someBankerQ = do
  sample $ (arbitrary :: Gen (BankerQueue Int))


testBankerQ :: IO ()
testBankerQ = do
  quickCheck (prop_front_rear_length :: BankerQueue Int -> Bool)
