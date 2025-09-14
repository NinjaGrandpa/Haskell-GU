-- Exercices Week 2 --
-----------------------------------------------------------------------------------------------------------------------------------------------------------
-- Self Check --

-- Average 
-- Answer: 
avg :: Fractional a => a -> a -> a
avg x y  = (x + y) / 2

-- The median Function:
-- Answer:
{-
Cases:
Unsorted Numbers
Sorted Numbers

123 = 2, x < y < z
321 = 2, x > y > z

213 = 2, x > y < z
231 = 2, x < y > z

312 = 2, x > y < z
132 = 2, x < y > z

 -}
 
median :: Ord a => a -> a -> a -> a
median x y z 
    | x < y && x < z = min y z
    | otherwise      = min x (max y z)
