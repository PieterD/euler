module Prime where

import Data.List(group)

divisibleBy :: Integer -> Integer -> Bool
divisibleBy x y = x `mod` y == 0

primes :: [Integer]
primes = 2 : sieve [x | x <- [3,5 ..]]
  where
    sieve (n:ns) = n : sieve (simpleSieve n ns)
    simpleSieve p (n:ns)
      | n `mod` p == 0 = simpleSieve p ns
      | otherwise = n : simpleSieve p ns

primeFactors :: Integer -> [Integer]
primeFactors n = removeFactors n primes
  where
    removeFactors 1 ps = []
    removeFactors n (p:ps)
      | n `mod` p == 0 = p : removeFactors (n `div` p) (p : ps)
      | otherwise = removeFactors n ps

allFactors :: Integer -> [Integer]
allFactors n = removeFactors [1 ..]
  where
    removeFactors (f:fs)
      | n == f * 2 = [f, n]
      | n `mod` f == 0 = f : removeFactors fs
      | otherwise = removeFactors fs

numDivisors :: Integer -> Int
numDivisors = product . map ((+1).length) . group . primeFactors