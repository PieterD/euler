module Assignments where

import           Prime
import           Useful

--------------------------
-- Multiples of 3 and 5 --
--------------------------
assignment1 = sum [x | x <- [1 .. 999], x `mod` 3 == 0 || x `mod` 5 == 0]

----------------------------
-- Even Fibonacci numbers --
----------------------------
assignment2 = sum [x | x <- takeWhile (\y -> y <= 4000000) fibonacci, even x]

--------------------------
-- Largest prime factor --
--------------------------
assignment3 = maximum . factors $ 600851475143

--------------------------------
-- Largest palindrome product --
--------------------------------
assignment4 = maximum $ filter (\x -> x == reverseNum x) [x * y | x <- [1 .. 999], y <- [1 .. 999]]
  where
    reverseNum = join 0 . revSplit
    revSplit 0 = []
    revSplit n = (n `mod` 10) : revSplit (n `div` 10)
    join s []     = s
    join s (n:ns) = join (s * 10 + n) ns

-----------------------
-- Smallest multiple --
-----------------------
assignment5 = product $ map (\(p, n) -> product $ replicate n p) factorList
  where
    base = [1 .. 20]
    highestNumberOfFactors n = maximum $ map (\b -> length $ filter (\f -> f == n) (factors b)) base
    factorList = map (\x -> (x, highestNumberOfFactors x)) . unique . factors . product $ base
