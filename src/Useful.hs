module Useful where

fibonacci = fibonacci' 1 2
  where
    fibonacci' a b = a : fibonacci' b (a + b)

unique [] = []
unique (x:xs) = unique' x xs
  where
    unique' y [] = [y]
    unique' y (x:xs)
      | y == x = unique' y xs
      | otherwise = y : unique' x xs
