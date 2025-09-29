-- ! Lecture 4B: more higher-order functions
{- 
-- ? Today
* Folding over a list:
    * foldr and fold1
* Function composition
* The higher-order function zipWith
* Manipulating functions:
    * flip
    * curry and uncuryy
    * The application operator (%) 
 -}

module V4.Lec4b where

import Prelude hiding (foldr, sum, product, concat, and, ord, maximum, zip, zipWith)

import Data.Char
import Data.List (group, sort)
import Test.QuickCheck

-- * The higher-order function foldr --

-- | sum [2,3,4] == 2+3+4 == 9
sum :: [Int] -> Int
sum []     = 0
sum (n:ns) = n + sum ns

-- | product [2,3,4] == 2*3*4 == 24
product :: [Int] -> Int
product []     = 1
product (n:ns) = n * product ns

-- | concat [[1,2],[],[3,4,5]] == [1,2] ++ [] ++ [3,4,5] == [1,2,3,4,5]
concat :: [[a]] -> [a]
concat []       = []
concat (xs:xss) = xs ++ concat xss

-- Identify the pattern, generalize: foldr
-- Parameters: how to apply results, base case, list
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr op b []     = b 
foldr op b (x:xs) = x `op` foldr op b xs

-- Reimplementing the above functions
sum' = foldr (+) 0
product' = foldr (*) 1
concat' = foldr (++) []

-- More functions: and, or, maximum (in eta reduced from)
and, or :: [Bool] -> Bool
and = foldr (&&) True
or  = foldr (||) False 

{- 
-- ? An intuition about foldr
* Think of foldr as a function that uses an operator to combine all the elements of a list:
    - foldr (+) 0 [5,6,7] == 5+6+7+0 == 18
* Remember that foldr (&) z (where & is some operator) is a function that:
    - replaces : with &, and
    - replaces [] with z

            xs == a : b : c : d : ... : []
foldr (&) z xs == a & b & c & d & ... & z
foldr (+) 0 xs == a + b + c + d + ... + 0
foldr (*) 1 xs == a * b * c * d * ... * 1

-- ? There is a foldl as well
* The foldl function is like foldr, but combines the first element with the base value,
* and then continues recursively
* foldr parenthesizes to the right, foldl to the left:
    - foldr (+) 0 [5,6,7] == 5+(6+(7+0))
    - foldl (+) 0 [5,6,7] == ((0+5)+6)+7

            xs == a : b : c : d : ... : []
foldl (&) z xs == z & a & b & c & d & ... 
foldl (+) 0 xs == 0 + a + b + c + d + ...
foldl (*) 1 xs == 1 * a * b * c * d * ...
 -}

-- maximum :: Ord => [a] -> a

-- A foldl example
-- | Slow reverse function
revSlow, revFast :: Ord a => [a] -> [a]
revSlow [] = []
revSlow (x:xs) = revSlow xs ++ [x]

-- | Fast reverse function
revFast xs = foldl (\acc x -> x:acc) [] xs
{- 
> revFast [1,2,3,4]
foldl (\acc x -> x:acc) [] [1,2,3,4]
[1] 

[] (\acc x -> x:acc) 1
([] (\acc x -> x:acc) 1) (\acc x -> x: acc) 2
(([] (\acc x -> x:acc) 1) (\acc x -> x: acc) 2) (\acc x -> x: acc) 3
((([] (\acc x -> x:acc) 1) (\acc x -> x: acc) 2) (\acc x -> x: acc) 3) (\acc x -> x: acc) 4
(((1:[]) (\acc x -> x: acc) 2) (\acc x -> x: acc) 3) (\acc x -> x: acc) 4
((2:(1:[])) (\acc x -> x: acc) 3) (\acc x -> x: acc) 4
(3:(2:(1:[]))) (\acc x -> x: acc) 4
4:(3:(2:(1:[])))
4:(3:(2:[1]))
4:(3:[2,1])
4:[3,2,1]
[4,3,2,1]

(1:[]):2
((1:[]):2):3
(((1:[]):2):3):4

3:[2,1]
4:[3,2,1]
[4,3,2,1]
 -}

-- * Function composition ----

-- Define composition `o` by hand. Notice that we need the parentheses around
-- the argument function (b -> c) and (a -> b), but we don't need to have them
-- around a -> c

-- | Takes one function and applies it to the other one
o :: (b -> c) -> (a -> b) -> (a -> c)
f `o` g = \x -> f (g x)

{- 
> f = filter (<4) . revFast
> f [1..10]
[3,2,1]
 -}

-- | removeSpaces "abc def \n ghi" = "abcdefghi"
removeSpaces :: String -> String
removeSpaces [] = []
removeSpaces (c:cs)
    | not (isSpace c) = c : removeSpaces cs
    | otherwise = removeSpaces cs

-- Redefine removeSpaces with composition
removeSpaces' :: String -> String
removeSpaces' str = filter (not . isSpace) str

-- A larger example: counting words in a string and produce nicely formatted
-- output, most common word first. For example:

-- wordCounts weather == "is: 3\nwarm: \2ncold: 1\nJune: 1\nJuly: 1\nJanuary: 1\n"

wordCount = unlines 
            . map (\(n, w) -> w ++ ": " ++ show n)
            . reverse 
            . sort 
            . map (\xs -> (length xs, head xs))
            . group 
            . sort 
            . words

weather :: String
weather = "June is warm\nJuly is warm\nJanuary is cold\n"

-- * zipWith, uncurry ----

-- Example: scalarProduct [4,5,6] [100,10,1] = 4*100 + 5*10 + 6*1 = 456
scalarProduct :: [Int] -> [Int] -> Int
scalarProduct xs ys = sum (map (\(x, y) -> x * y) (zip xs ys))

-- Let's generalize zip by taking an extra parameter (function), that combines
-- the two elements of the input lists.
zip :: [a] -> [b] -> [(a, b)]
zip (x:xs) (y:ys) = (x, y) : zip xs ys
zip _      _      = []

-- Generalize to zipWith
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys
zipWith _ _ _           = []

-- Redefine zip
-- zip' :: [a] -> [b] -> [(a, b)]

-- * Function application ($), flip, uncurry and curry ----

-- Define reverse using foldl and flip
-- flip :: flip :: (a -> b -> c) -> b -> a -> c
-- foldl ::  Foldable t => (b -> a -> b) -> b -> t a -> b

rev' :: [a] -> [a]
-- rev' foldl (\acc x -> x:acc) []
rev' foldl (flip (:)) []

-- * Using function in a data type declaration

data ListAlg a b = Alg 
    {
        cons :: a -> b -> b
    ,   nil :: b
    }