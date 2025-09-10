import Prelude hiding (take, drop, zip, maximum, reverse)

{- Lecture 2C: more list functions
    and accumulating parameters -}
-- Recap:
-- Polymorphic fumctions
-- Strings are lists!
-- Common type classes:
-- -- Used for overloading
-- -- Show, Eq, Ord, Num

-- Today:
-- More list functions
-- -- With multiple arguments
-- Extra and accumulating parameters
-- Property-based testing with QuickCheck
-- Building an executable
-- Local definitions

----------------------------------------------------------------------------------------------------------------------------------
-- Compiling a program:
-- * ghc -- make
-- * show runghc
-- * -main-is <main>

main = print "Hello, World!"

----------------------------------------------------------------------------------------------------------------------------------
-- Local definitions
-- * where clauses
-- * let-expressions

inc :: Int -> Int
inc x = x + 1

-- We can have local definitions in a where-clause
addN :: Int -> [Int] -> [Int]
addN n xs = [inc x | x <- xs]
    where
        inc x = x + n

-- Alternatively using a let-expression:
addM :: Int -> [Int] -> [Int]
addM m xs = let inc x = x + m in [inc x | x <- xs]

----------------------------------------------------------------------------------------------------------------------------------
-- Recursive functions with multiple lists

append :: [a] -> [a] -> [a]
append [] ys = ys
append (x:xs) ys = x : append xs ys 
-- Linjär i första argument, om första listan är kort så går det fort och tvärtom om den är lång

{- Example:
ghci> append [1,2,3,4] [5,6,7,8]
[1,2,3,4,5,6,7,8] 

append [1,2] [3,4]
append (1 : 2 : []) [3,4]
1 : append (2 : []) [3,4]
1 : 2 : append [] [3,4]
1 : 2 : [3,4]
[1,2,3,4]
-}

-- take and drop --
-- Take the first n elements from a list. Example : take 2 "Haskell" == "Ha"

take :: Int -> [a] -> [a]
take n (x:xs) | n > 0 = x : take (n-1) xs -- If n > and x has an element
take _ _ = [] -- In every other case

-- Drop the first n elements from a list. Example : drop 2 "Java" = "va"
drop :: Int -> [a] -> [a]
drop n (x:xs) | n > 0 = drop (n-1) xs

-- zip and unzip --

-- Combining two lists into a list of pairs. Example zip [1,2,3] [10,20] = [(1,10),(2,20)]
zip :: [a] -> [b] -> [(a, b)]
zip (x:xs) (y:ys) = (x, y) : zip xs ys
zip _ _ = []

-- Unzipping a list of pairs into a pair of lists. Example: unzip [(1,10), (2,20), (3,30)] = ([1,2], [10,20])
-- unzip :: [(a, b)] -> ([a], [b])
-- unzip = undefined

-- testUnzip = unzip [(1, 10), (2, 20), (3,30)] == ([1,2], [10, 20])

----------------------------------------------------------------------------------------------------------------------------------
-- Accumulating parameters

-- Sparas som globala variabler i Imperativa språk

maximum :: [Int] -> Int
maximum (y:ys) = go y ys 
    where
     go m [] = m
     go m (x:xs) = go (max m x) xs  

-- reverse (same as last time)
reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

-- Kvadratisk antal steg att beräkna, lista med 10 element behöver 100 kalkylationer

-- Faster reverse, accumulating parameter
revFast :: [a] -> [a]
revFast xs = go [] xs
    where
        go acc [] = acc
        go acc (x:xs) = go (x:acc) xs

----------------------------------------------------------------------------------------------------------------------------------
-- Property-based testing
-- * test the quicksort sorting algorithm

-- Sorting a list using the quicksort algorithm. It has a different recursion pattern.
sort :: Ord a => [a] -> [a]
sort [] = []
sort (p:xs) = sort smaller ++ [p] ++ sort larger
    where
        smaller = [x | x <- xs, x < p]
        larger = [x | x <- xs, x >= p]

-- properties
prop_idem :: [Int] -> Bool
prop_idem xs = sort xs == sort (sort xs)

-- Test with quickCheck

prop_permutation :: [Int] -> Bool
prop_permutation xs = null (xs \\ ys) && length xs == length ys
    where
        ys = sort xs

sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x,y) <- zip xs (tail xs)]

prop_sorted :: [Int] -> Property
prop_sorted 