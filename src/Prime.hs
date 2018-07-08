module Prime
  ( allFactors
  , numDivisors
  , primeFactors
  , primes
  ) where

import Data.List(mapAccumL)
import qualified Data.Heap as H (MinPrioHeap, insert, singleton, splitAt)
import           Data.List (group)

allFactors :: Integer -> [Integer]
allFactors n = removeFactors [1 ..]
  where
    removeFactors (f:fs)
      | n == f * 2 = [f, n]
      | n `mod` f == 0 = f : removeFactors fs
      | otherwise = removeFactors fs

numDivisors :: Integer -> Int
numDivisors = product . map ((+ 1) . length) . group . primeFactors

primeFactors :: Integer -> [Integer]
primeFactors n = removeFactors n primes
  where
    removeFactors 1 ps = []
    removeFactors n (p:ps)
      | n `mod` p == 0 = p : removeFactors (n `div` p) (p : ps)
      | otherwise = removeFactors n ps

-- A very simple prime sieve.
-- No optimizations, beside skipping even numbers.
-- Very slow. Used ghci :set +s as a crude performance measure.
primes'' :: [Integer]
primes'' = 2 : sieve [x | x <- [3,5 ..]]
  where
    sieve (n:ns) = n : sieve (simpleSieve n ns)
    simpleSieve p (n:ns)
      | n `mod` p == 0 = simpleSieve p ns
      | otherwise = n : simpleSieve p ns

-- An enormously faster prime sieve.
-- Skips known composites using a priority queue or prime multiples.
primes' :: [Integer]
primes' = 2 : 3 : sieve [x | x <- [5,7 ..]] initHeap
  where
    initHeap = H.singleton (3 * 3, 3) :: H.MinPrioHeap Integer Integer
    sieve :: [Integer] -> H.MinPrioHeap Integer Integer -> [Integer]
    sieve (n:ns) heap
      | n < pp = n : sieve ns newPrime
      | n == pp = sieve ns notPrime
      | n > pp = sieve (n : ns) notPrime
      where
        ((pp, p):[], heap') = H.splitAt 1 heap
        newPrime = H.insert (n * n, n) heap
        notPrime = H.insert (pp + p, p) heap'

-- An even faster prime sieve.
-- Uses wheel factorization to skip all multiples of the first 5 primes outright.
primes :: [Integer]
primes = firstPrimes ++ nextPrime : sieve fullWheel initHeap
  where
    (firstPrimes, nextPrime, primeWheel) = wheel 5
    fullWheel = tail . scanl (+) nextPrime . cycle $ primeWheel
    initHeap = H.singleton (nextPrime * nextPrime, nextPrime) :: H.MinPrioHeap Integer Integer
    sieve :: [Integer] -> H.MinPrioHeap Integer Integer -> [Integer]
    sieve (n:ns) heap
      | n < pp = n : sieve ns newPrime
      | n == pp = sieve ns notPrime
      | n > pp = sieve (n : ns) notPrime
      where
        ((pp, p):[], heap') = H.splitAt 1 heap
        newPrime = H.insert (n * n, n) heap
        notPrime = H.insert (pp + p, p) heap'

-- Calculate prime wheel
wheel :: Int -> ([Integer], Integer, [Integer])
wheel n = (firstPrimes, nextPrime, summy . diffy $ deFactor)
  where
    (firstPrimes, nextPrime:_) = splitAt n primes'
    prod = product firstPrimes
    -- Return an infinity list of integers (starting at 2) with all multiples of the first n primes removed.
    deFactor = filter isDivisible [2 ..]
      where
        firstPrimes = take n primes'
        isDivisible x = (== 0) . length . filter (\p -> x `mod` p == 0) $ firstPrimes
    -- Turn the above list into a list of differences
    diffy (x1:x2:xs) = (x2 - x1) : diffy (x2 : xs)
    -- Return the section of the above list where the sum of the element is equal to n
    summy ps = summy' 1
      where
        summy' x
          | sum takex >= prod = takex
          | otherwise = summy' (x + 1)
          where
            takex = take x ps
