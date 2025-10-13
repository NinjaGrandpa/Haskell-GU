-- ! Lecture 4A: Higher-order functions --

-- ? Recap previouse lecture
-- * Input/output (IO) in Haskell
    -- Printing and getting input from userrs
    -- Reading to and writing from file
    -- do-notation
-- * Generating test data using QuickCheck
    -- Generator combinators
    -- do-notation
    -- Type class Arbitrary
    -- Testing properties

-- ? Today
-- * Generalisation/abstraction
-- * Higher-order functions:
    -- map
    -- filter
-- * Working with functions
    -- Type synonyms for functions
    -- Programming with regions
-- * A more general sort function


-- ? How to be productive as a programmer
-- * Use general-purpose functions from the standard libraries when possible
-- * Create general, re-usable funcions and use them many times
    -- Identify common patterns
    -- Avoid copying and modifying code
-- * Use libraries from other sources (e.g. from Hackage)
    -- But...finding and learning how to use libraries can also takes time
    -- But...dependencies can be a nightmare
-- * Less code
    -- => fewer bugs
        -- => lower maintenance burden

-- ? How to make functions more general
-- * Add extra parameters to make functions more versatile
-- * Create functions that work for many types instead of just one
 -- Polymorphic functions, also known as generic functiosn
 -- Overloaded functions, which work for a range of types
-- * Create higher-order functions
    -- A key ingredient in functional programming
    -- The main topic today

-- ? Higher-order functions
-- * A function is called higher-order if it takes a function as an argument
-- * Common programming idioms can be encoded as functions within the language itself.
-- * Domain specific languages can be defined as collections of higher-order functions.

module Lec4a where
import Prelude hiding (map, filter)
import Data.List (partition)
import Lec3b


-- | twice is higher-order because it takes a function as its first argument
twice :: (a -> a) -> a -> a
twice f x = f (f x)

-- * We use the familiar data types for card in the examples below --
hand :: Hand
hand = [Card Jack Clubs, Card Queen Hearts, Card King Clubs, Card Ace Spades]

-- * Examples of reducing code size by making functions more general

-- ** Bad programming style
-- Counting the number of clubs in a hand
countClubs :: Hand -> Int
countClubs hand = length [suit | Card _ suit <- hand, suit == Clubs]

-- Counting the number spades in a hand
countSpades :: Hand -> Int
countSpades hand = length [suit | Card _ suit <- hand, suit == Spades]

-- Generalize!
countSuit :: Suit -> Hand -> Int
countSuit s hand = length [suit | Card _ suit <- hand, suit == s]

-- Reimplementing the above functions
-- Eta-reduced by removing hand parameter 
countClubs', countSpades' :: Hand -> Int
countClubs' = countSuit Clubs 
countSpades' = countSuit Spades

-- * Identifying patterns an generalize, introduce higher-order functions

-- Double every number in a list, doubles [1,2,3] == [2,4,6]
doubles :: [Int] -> [Int]
doubles []     = [] 
doubles (x:xs) = 2 * x : doubles xs

-- Compute the squares of all numbers in a list, squares [1,2,3] == [1,4,9]
squares :: [Int] -> [Int]
squares [] = []
squares (x:xs) = x^2 : squares xs

-- Identify the pattern, generalize: map
map :: (a -> b) -> [a] -> [b]
map f []     = []
map f (x:xs) = f x : map f xs

-- Alternative implementation

-- Reimplement, functions as argument!!!!

-- doubles1 xs = map (\x -> 2*x) xs
doubles1 = map (2*)  -- eta-reduced and use sections
squares1 = map (^2)

-- * Passing functions as arguments to higher-order functions ------

-- Using an existing function
-- sizes :: [String] -> [Int]


-- Defining a helper function in a where clause (or let-expression)
squares3 :: [Int] -> [Int]
squares3 = let sq x = x^2 in map sq
-- squares3 xs = let sq x = x^2 in map sq xs -- eta-expanded

-- * Another example: filter -----
-- | Filter out all uneven numbers: evens [1..10] == [2,4,6,8,10]
evens :: [Int] -> [Int]
evens [] = []
evens (x:xs) 
    | even x    = x : evens xs
    | otherwise = evens xs

-- | Keep all elements larger than n: largerThan 7 [1..10] == [8,9,10]
largerThan :: Int -> [Int] -> [Int]
largerThan _ [] = []
largerThan n (x:xs)
    | x > n     = x : largerThan n xs
    | otherwise = largerThan n xs

-- Identify pattern and generalize: filter
filter :: (a -> Bool) -> [a] -> [a]
filter p []  = []
filter p (x:xs)
    | p x = x : filter p xs
    | otherwise = filter p xs

-- Reimplement
evens2 = filter even
largerThan2 n = filter (>n)

-- * More example son higher-order functions -----

data Person = P { name :: String, age :: Int, credit :: Int } deriving Show -- Defined using Record syntax

pers :: [Person]
pers = [P "Max" 23 0, P "Kajsa" 21 1234, P "Molly" 13 654321]

-- Defining a more general sort function: the comparison function is an argument
-- to the function.

-- partition: partitions a list based on a predicate, a higher-order function

sortBy :: (a -> a -> Bool) -> [a] -> [a]
sortBy cmp = qsort
  where
    qsort [] = []
    qsort (p:xs) = qsort smaller ++ [p] ++ qsort larger
        where (smaller, larger) = partition (\x -> x `cmp` p) xs

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f = sortBy (\x y -> f x <= f y)

