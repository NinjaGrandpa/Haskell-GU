import Prelude hiding (take, drop)

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
drop n (x:xs) | n > 0 = 