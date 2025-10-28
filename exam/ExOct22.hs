module ExOct22 where

-- * Part 1.

-- ** 1.
f x (y:z:zs) = y : x : f x (z:zs)
f _ xs = xs

g x xs = concat (f x xs)

xs = ["intro", "func", "prog"]

-- *** a)
{- 
g "" ["intro", "func", "prog"]
= applying g
concat (f "" ["intro", "func", "prog"])
= applying f
concat $ "i" : " " : f "" ["func", "prog"] 


 -}

-- *** b)