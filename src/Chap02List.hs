module Chap02List where

import Data.List (isSuffixOf)
import Test.QuickCheck (quickCheck)

-- exercise 2.1
suffix :: [a] -> [[a]]
suffix [] = [[]]
suffix a@(x:xs) = a : suffix xs

-- length property
prop_len xs = length xs + 1 == length (suffix xs)
-- in property
prop_in [] = True
prop_in xs =
  case suffix xs of
    [] -> True
    [xs] -> True
    suffix_xs -> all (`isSuffixOf` xs) suffix_xs
-- empty list is a member of the suffix
prop_empty xs = [] `elem` suffix xs


suffixTest :: IO ()
suffixTest = do
  quickCheck (prop_len :: [Int] -> Bool)
  quickCheck (prop_in :: [String] -> Bool)
  quickCheck (prop_empty :: [Bool] -> Bool)
