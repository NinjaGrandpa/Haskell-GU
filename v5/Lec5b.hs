{- |
Module      : Lecture5B
Description : Lecture week 5, part B, catch our breath
Copyright   : (c) 2025 TDA555/DIT441, Introduction to Functional Programming
License     : BSD
Maintainer  : alexg@chalmers.se
Stability   : experimental

Catching-our-breath lecture.
-}

module Main where

import Test.QuickCheck

-- * Compiling source code -----------------------------------------------------

main :: IO ()
main = putStrLn "Lecture 5B, catch our breath"

-- * Data types, types, functions, guards, instance, pattern matching ----------

-- Create a student data type with: name, social num, email, and completed 
-- courses along with the grade. Use record syntax.

data Student = Student
  { name      :: String
  , socialNum :: Int
  , email     :: String
  , courses   :: [(Course, Grade)]
  } 

-- Write a Show instance for the student data type
instance Show Student where
    show (Student n sn _ _) = n ++ " (" ++ show sn ++ ")"

instance Eq Student where 
    s1 == s2 = socialNum s1 == socialNum s2

-- Define a data type for a grade together with a Show instance
data Grade = U | Three | Four | Five deriving Eq

-- Make a Show instance for Grade
instance Show Grade where
    show grade = case grade of
        U     -> "U"
        Three -> "3"
        Four  -> "4"
        Five  -> "5"

-- Implement a course data type with: code, credits, prerequisites
data Course = Course
 { code      :: Code
 , credits :: Double
 , prereqs :: [Course]
 }

-- Show instance for Course, use function composition
instance Show Course where
    show = show . code

-- Define Eq instance for course data type, just compare code
instance Eq Course where
    c1 == c2 = code c1 == code c2

-- Data type for course codes, no string for code but an own data type,
-- differentiate between cth, gu, and joint course.
data Code = CTH String | GU String | SAM String String deriving Eq

instance Show Code where 
    show (CTH c) = c
    show (GU c) = c
    show (SAM c1 c2) = c1 ++ "-" ++ c2

-- Example data
tda555, dit962, dit013 :: Course
tda555 = Course (SAM "TDA555" "DIT441") 7.5 []
dit962 = Course (GU "DIT961") 7.5 [tda555]
dit013 = Course (GU "DIT013") 6 []

mats, lise, sofie :: Student
mats  = Student "Mats"  1234 "mats@gerdes.nl" [(tda555, U)]
lise  = Student "Lise"  3132 "lise@gerdes.nl" [(tda555, Three), (dit013, Four)]
sofie = Student "Sofie" 6586 "sofie@gerdes.nl" []


-- * Defining functions --------------------------------------------------------

-- Retrieve all courses passed by a given student. Use map and filter, composition
-- and a where clause with a help function.
passedCourses :: Student -> [Course]
passedCourses = map fst . filter p . courses
  where
    p (c, g) = g /= U

-- Check if a student is allowed to read a given course. Use list comprehension.
-- canRead :: Student -> Course -> Bool 

-- The most important function of all! How much CSN does a student get?!?
-- Use guards.
-- csn :: Student -> Double

-- Write a function that adds a course with grade to a student. A choice: allow
-- multiple grades or not.
-- grade :: Course -> Grade -> Student -> Student

-- * Input/Output --------------------------------------------------------------

-- Write a function that asks for a student information and creates a Student.
-- readStudent :: IO Student

-- * Recursive data type, higher-order functions -------------------------------

-- Model a row in a class room.
-- data Row a

-- Smart constructors
-- (<|) :: a -> Row a -> Row a

-- (|>) :: Row a -> a -> Row a

-- Fix fixity
-- infixr 6 <|
-- infixl 5 |>

-- Example row
-- row :: Row Student
-- row = mats <| Empty |> sofie |> lise

-- Map a function over a row
-- mapRow :: (a -> b) -> Row a -> Row b


-- Write a function that folds a 'Row', think about the 'right' case!
-- foldRow :: (a -> b -> b) -> b -> Row a -> b
  
-- Implement a function that converts a 'Row' to a list. Note that the order of
-- the elements should be unchanged! Of course, use a fold.
-- toList :: Row a -> [a]

-- Start of a row
-- start :: Row a -> a

-- Define a function that returns the last element from a 'Row' in a 'Maybe'
-- end :: Row a -> Maybe a

-- * QuickCheck (test data generation) -----------------------------------------

-- Write a generator for random 'Row's. It is desirable to add elements randomly
-- to the front and back of the row. Hint: use a list of 'Bool' to decide 
-- whether to add them to the back or front.
-- genRow :: Arbitrary a => Gen (Row a)

-- Make an 'Arbitrary' instance declaration for the type 'Row'.
-- instance Arbitrary a => Arbitrary (Row a) where

-- Write a property that check the 'first' and 'last' function on 'Row's.
-- prop_start_end :: Int -> Int -> Row Int -> Bool

-- prop_order :: [Int] -> InfiniteList Int -> Bool
 
