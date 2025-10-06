{-# LANGUAGE TemplateHaskell #-}
module Exv3 where
-- ! Exercices week 3: data types and list functions --

import Prelude hiding (take, unzip)
import Test.QuickCheck
import Test.QuickCheck.All

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
-- data Person = Person [String] String Int deriving (Show, Eq)
data Person = Person
 { firstnames :: [String]
 , lastname :: String
 , age :: Int
 } deriving (Show, Eq)

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
named name ps = [p | p@(Person fns _ _) <- ps, name `elem` fns]

-- | Selects persons using age
ages :: Int -> [Person] -> [Person]
ages a ps = [p | p@(Person _ _ age) <- ps, age == a]

-- | Creates a list of pairs 
pairs :: [a] -> [b] -> [(a,b)]
pairs xs ys = [(x, y) | x <- xs, y <- ys]

-- ? Pythagorean triad

-- | Pythagorean triad
pythTri :: [(Int, Int, Int)]
pythTri = [(a, b, c) | a <- [1..100], b <- [1..100], c <- [1..100], a^2 + b^2 == c^2]

prop_pythTri = and $ map (\(a, b, c) -> c^2 == (a^2 + b^2)) pythTri

-- ? Defining functions over lists

-- | take
take :: Int -> [a] -> [a]
take n _ | n <= 0 = []
take _ []         = []
take n (x:xs)     = x : take (n-1) xs

-- | Split a list into two at a given index
splitAt' :: Int -> [a] -> ([a], [a])
splitAt' n xs | n <= 0 = ([], xs)
splitAt' _ []          = ([], [])
splitAt' n (x:xs)      = 
    let (ys, zs) = splitAt' (n-1) xs
    in (x : ys, zs)

prop_splitAt :: Eq a => Int -> [a] -> Bool
prop_splitAt n xs = splitAt n xs == splitAt' n xs

-- ? Permutations

-- | Check if a list is a permutation of another list
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation [] _ = False 
isPermutation _ [] = False 
isPermutation (x:xs) ys = isPermutation xs $ removeOnce x ys

-- | Removes an element from a list once
removeOnce :: Eq a => a -> [a] -> [a]
removeOnce _ [] = []
removeOnce e (x:xs) | e == x = xs
                    | otherwise = x:removeOnce e xs 

-- ? Unzipping and writing properties

-- | Takes a list of and returns a pair of lists
unzip :: [(a, b)] -> ([a], [b])
unzip xs = ([fst a | a <- xs], [snd b | b <- xs])

prop_unzip_testA = unzip [(1, 'a'), (2, 'b'), (3, 'c')] == ([1,2,3], "abc")

prop_unzip_same :: (Eq a, Eq b) => [(a, b)] -> Bool
prop_unzip_same xs = let (a, b) = unzip xs in xs == zip a b

prop_unzip_length :: (Eq a, Eq b) => [(a, b)] -> Bool
prop_unzip_length xs = let (a, b) = unzip xs in length xs == length a && length xs == length b

-- ? Modeling students
data Student = Student String String Bool

instance Show Student where
    show (Student n p csn) = n ++ " studies " ++ p ++ " and does" ++ 
                             (if csn then "" else " not") ++ " have CSN."

instance Eq Student where
    Student n1 _ _ == Student n2 _ _ = n1 == n2

prop_student_show, prop_student_eq :: Bool
prop_student_show = "Alex studies Datavetenskap and does not have CSN." == show (Student "Alex" "Datavetenskap" False)
prop_student_eq = (Student "Alex" "Datavetenskap" False) == (Student "Alex" "Någotannat" True)

---------------------------------------------------------------------------------------------------------------------------------
-- * Advanced Questions

-- ? Defining Types

data Month = January 
           | February 
           | March 
           | April 
           | May 
           | June 
           | July 
           | August 
           | September 
           | October 
           | November 
           | December
           deriving (Eq, Show, Ord, Bounded, Enum)


-- | Calculates if the current year is a leap year or not
isLeapYear :: Integer -> Bool
isLeapYear y
  | (y `mod` 4 == 0 && y `mod` 100 /= 0) || (y `mod` 400 == 0) = True
  | otherwise = False

-- | Computes the number of days in a month, given the year
daysInMonth :: Month -> Integer -> Integer
daysInMonth m y 
    | m == February = if isLeapYear y then 29 else 28
    | isEven && isAfterAug || not isEven && not isAfterAug = 31
    | otherwise = 30
  where
    isAfterAug = (fromEnum m + 1) >= 8
    isEven = (fromEnum m + 1) `mod` 2 == 0

data Date = Date 
    { year :: Integer 
    , month :: Month 
    , day :: Integer
    } deriving (Eq)

instance Show Date where 
    show (Date year month day) = (ordinal day) ++ " of " ++ show month ++ " " ++ show year

ordinal :: Integer -> String
ordinal d = case d of
    1 -> "1st"
    2 -> "2nd"
    3 -> "3rd"
    _ -> show d ++ "th"

-- | Checks if date is between 1 and number of days in month
validDate :: Date -> Bool
validDate (Date year month day) = day > 0 && day <= daysInMonth month year

-- | Returns tomorrow's date for a given date
tomorrow :: Date -> Date
tomorrow date@(Date y m d) 
    | not (validDate date) = error "Date is not valid"
    | d == daysInMonth m y = 
        if m == December then (Date (y+1) January 1)
        else date { day = 1, month = (succ m) }
    | otherwise = date { day = d + 1 }

-- ? Pascal's Triangle

-- ? Erastosthenes' sieve

-- ? Occurrences in Lists

-- ? Number Games

-- ? Sorting

return []
runTests :: IO Bool
runTests = $quickCheckAll