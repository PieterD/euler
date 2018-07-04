module Prime where

divisibleBy :: Integer -> Integer -> Bool
divisibleBy x y = x `mod` y == 0

primes :: [Integer]
primes = 2 : sieve [x | x <- [3,5 ..]]
  where
    sieve (n:ns) = n : sieve (simpleSieve n ns)
    simpleSieve p (n:ns)
      | n `mod` p == 0 = simpleSieve p ns
      | otherwise = n : simpleSieve p ns

factors :: Integer -> [Integer]
factors n = removeFactors n primes
  where
    removeFactors 1 ps = []
    removeFactors n (p:ps)
      | n `mod` p == 0 = p : removeFactors (n `div` p) (p : ps)
      | otherwise = removeFactors n ps
