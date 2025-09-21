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
data Person = Person {
    firstNames :: [String],
    lastNames :: [String],
    ages :: [Int]
} deriving (Show)

kjellgren, jenslov :: Person
kjellgren = Person ["Kajsa", "Maja", "Olle", "Lars", "Ulrica"] ["Kjellgren"] [21, 23, 17, 0, 0]
jenslov = Person ["Max", "Molly", "Mira", "Ulrika", "Niklas"] ["Jenslöv"] [23, 14, 11, 50, 53]

families = [kjellgren, jenslov] :: [Person]

-- ? Extra parameters
-- | Calculates maximum and minimum of a non-empty list
minmax :: Ord a => [a] -> (a, a)
minmax (y:ys) = go (y, y) ys
    where
        go (mn, mx) [] = (mn, mx)
        go (mn, mx) (x:xs) = go ((min mn x), (max mx x)) xs

---------------------------------------------------------------------------------------------------------------------------------
-- * Basic


---------------------------------------------------------------------------------------------------------------------------------
-- * Challenging

