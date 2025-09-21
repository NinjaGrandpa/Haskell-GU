-- ! Exercices week 3: data types and list functions --

import Data.List

---------------------------------------------------------------------------------------------------------------------------------
-- * Self check

-- ? Warming up
-- | Multiplies all elements in a list
prod :: [Int] -> Int
prod [] = 1
prod (n:ns) = n * prod ns 

-- | Selects all elements on uneven indexes
odds :: [a] -> [a]
odds xs = [x | (x,i) <- zip xs [0..], odd i]

prop_oddsnums = odds [0,1,2,3,4,5,6] == [1,3,5] 

-- ? Weekdays
data Weekday = 
    Monday |
    Tuesday | 
    Wednesday | 
    Thursday | 
    Friday | 
    Saturday | 
    Sunday deriving (Show, Eq)

weekday :: Int -> Weekday
weekday 1 = Monday
weekday 2 = Tuesday
weekday 3 = Wednesday
weekday 4 = Thursday
weekday 5 = Friday
weekday 6 = Saturday
weekday 7 = Sunday

-- ? Person data type
data Person = Person [String] String Int deriving (Show, Eq)

maxj, molly, mira :: Person
maxj = Person ["Max", "Vilgot"] "Jenslöv" 23
molly = Person ["Molly", "Viola"] "Jenslöv" 13
mira = Person ["Mira", "Marianne"] "Jenslöv" 11

fam = [maxj, molly, mira] :: [Person]

-- ? Extra parameters
-- | Calculates maximum and minimum of a non-empty list
minmax :: Ord a => [a] -> (a, a)
minmax (y:ys) = go (y, y) ys
    where
        go (mn, mx) [] = (mn, mx)
        go (mn, mx) (x:xs) = go ((min mn x), (max mx x)) xs

-- ? Watch your head
-- | Safe head function
safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead xs = Just (head xs)

-- ? List Comprehensions
-- | Return a list with elements greater than n and smalller than m
between :: Int -> Int -> [Int] -> [Int]
-- between n m xs = filter (\x -> (x > n && x < m)) xs
between n m xs = [x | x <- xs, (x > n && x < m)]

---------------------------------------------------------------------------------------------------------------------------------
-- * Basic questions

-- ? More list comprehensions
-- | Selects persons with a given name
named :: String -> [Person] -> [Person]
named name ps = [Person fns ln age | Person fns ln age <- ps, name `elem` fns]

ages :: Int -> [Person] -> [Person]
ages age ps = []

-- findPeople :: ((Foldable t, Eq a) => a -> t a -> Bool) -> [Person] -> [Person]
-- findPeople x ps = [Person fns ln age | Person fns ln age <- ps, x] 

---------------------------------------------------------------------------------------------------------------------------------
-- * Challenging

