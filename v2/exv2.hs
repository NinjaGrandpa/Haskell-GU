import Test.QuickCheck

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

-- Greatest Common Divisor
-- Answer:
gcdiv :: Int -> Int -> Int
gcdiv x 0 = x
gcdiv x y = gcdiv y (x `mod` y)

-- Dice
-- Answer:
dice :: Int -> [(Int, Int)]
dice n = [(x, y) | x <- [1..(n-1)], x <= 6, y <- [(n-x)], y <=6]

dice' :: Int -> [(Int, Int)]
dice' n = [(x, y) | x <- [1..6], y <- [1..6], x + y == n]

test_dice n = dice n == dice' n

-----------------------------------------------------------------------------------------------------------------------------------------------------------
-- Basic Questions --

-- Price Calculation
{- 
price of prawns = 270/kg
weight > 50kg = 20% per kg
weight > 100kg = 30% per kg
-}
-- Answer:
priceDiscount :: (Fractional a, Num a, Ord a) => a -> a
priceDiscount kg | kg > 100 = 270 * 0.7 * kg
                 | kg > 50 = 270 * 0.8 * kg
                 | otherwise = 270 * kg

prop_disc50 = priceDiscount 50 == 13500
prop_disc51 = priceDiscount 51 == 11016.0
prop_disc101 = priceDiscount 101 == 19089.0

-- Sum of Squares
{- 
sumsq 3 
= 1*1 + 2*2 + 3*3
= 1 + 4 + 9
= 14
 -}

-- Answer:
sumsq :: Integral a => a -> a
sumsq 0 = 0
sumsq n = n*n + sumsq (n-1)
-- sumsq n = sum [x*x | x <- [1..n]]

prop_sumsq :: Int -> Bool
prop_sumsq n = sumsq n == (n * (n + 1) * (2 * n + 1) `div` 6)
prop_sumsq3 = sumsq 3 == 14

-----------------------------------------------------------------------------------------------------------------------------------------------------------
-- More Challenging Questions

-- The Towers of Hanoi

{- 
3 posts
n rings
From left to rightmost post
Smaller ring can't be on top of larger ring
n = 1, result 1
n = 2, result 3
n = 3, result = 7
n = 4, result = 15
n = 5, result = 31
n = 6, result = 63
n = 8, result = 255

h = 2^n-1
 -}

-- Answer:
hanoi :: Int -> Int
hanoi 1 = 1
hanoi n = (2^n)-1

prop_hanoi3 = hanoi 3 == 7
prop_hanoi5 = hanoi 5 == 31
prop_hanoi8 = hanoi 8 == 255