module NinetyNine where

--------------------------------------
-- Find the last element of a list. --
--------------------------------------
assignment1 = lastElement [1,2,3,4]
  where
    lastElement [x] = x
    lastElement (x:xs) = lastElement xs

----------------------------------------------
-- Find the last but one element of a list. --
----------------------------------------------
assignment2 = secondToLastElement [1,2,3,4]
  where
    secondToLastElement [x,_] = x
    secondToLastElement (x:xs) = secondToLastElement xs


