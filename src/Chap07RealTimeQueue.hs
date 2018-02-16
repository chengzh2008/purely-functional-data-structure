{-# LANGUAGE BangPatterns #-}
module Chap07RealTimeQueue where

import Queue

data RealTimeQueue a = RTQ { front :: [a]
                       , rear :: [a]
                       , accu :: [a]
                       } deriving Show

rotate :: [a] -> [a] -> [a] -> [a]
-- base case: front is empty, rear is sington list
rotate [] (y:[]) accu = y:accu
rotate (x:xs) (y:ys) accu = x:(rotate xs ys $ y:accu)

exec :: RealTimeQueue a -> RealTimeQueue a
exec (RTQ front rear accu) = case accu of
  x:s -> RTQ front rear s
  [] -> let !front' = rotate front rear [] in RTQ front' [] front'

instance Queue RealTimeQueue where
  empty = RTQ [] [] []
  isEmpty (RTQ front rear accu) = case front of
    [] -> True
    _ -> False

  snoc (RTQ front rear accu) x = exec $ RTQ front (x:rear) accu
  headq rtq@(RTQ front rear accu) = case isEmpty rtq of
    True -> error "empty queue"
    _ -> head front

  tailq rtq@(RTQ front rear accu) = case isEmpty rtq of
    True -> error "empty queue"
    _ -> exec $ RTQ (tail front) rear accu
