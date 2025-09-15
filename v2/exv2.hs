-- Exercices Week 2 --
-----------------------------------------------------------------------------------------------------------------------------------------------------------
-- Self Check --

-- Average 
-- Answer: 
avg :: Fractional a => a -> a -> a
avg x y  = (x + y) / 2

-- The median Function:
-- Answer: 
median :: Ord a => a -> a -> a -> a
median x y z 
    | x < y && x < z = min y z
    | otherwise      = min x (max y z)

-- Exclusive or (XOR)
-- Answer:

xor :: Bool -> Bool -> Bool
xor a b  = a /= b
