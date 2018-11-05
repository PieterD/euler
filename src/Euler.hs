module Euler where

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
assignment3 = maximum . primeFactors $ 600851475143

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
    highestNumberOfFactors n = maximum $ map (\b -> length $ filter (\f -> f == n) (primeFactors b)) base
    factorList = map (\x -> (x, highestNumberOfFactors x)) . unique . primeFactors . product $ base

---------------------------
-- Sum square difference --
---------------------------
assignment6 = squareOfSums - sumOfSquares
  where
    sumOfSquares = sum . map (\x -> x * x) $ [1 .. 100]
    squareOfSums = (^ 2) . sum $ [1 .. 100]

-------------------
-- 10001st prime --
-------------------
assignment7 = primes !! 10000

---------------------------------
-- Largest product in a series --
---------------------------------
assignment8 = error "Nope!"

---------------------------------
-- Special pythagorean triplet --
---------------------------------
-- a+b+c = 1000, a^2+b^2 = c^2, return abc
assignment9 =
  head [a * b * c | a <- [1 .. 1000], b <- [1 .. 1000], c <- [1 .. 1000], a ^ 2 + b ^ 2 == c ^ 2, a + b + c == 1000]

-------------------------
-- Summation of primes --
-------------------------
assignment10 = sum . takeWhile (< 2000000) $ primes

-------------------------------
-- Largest product in a grid --
-------------------------------
assignment11 = error "Nope!"

--------------------------------------
-- Highly divisible triangle number --
--------------------------------------
assignment12 = take 1 . filter ((>= 500) . numDivisors) $ triangleNumbers
  where
    triangleNumbers = [sum [1 .. x] | x <- [1 ..]]

---------------
-- Large sum --
---------------
assignment13 = error "Nope!"
