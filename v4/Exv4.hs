module V4.Exv4 where

import Test.QuickCheck
import Data.Maybe (isNothing, isJust)
import Data.Char (isAlpha, toLower)
import Data.List (notElem)

-- ! Excercises week 4: IO and testing --

-- ** Self-check
-- ? Basic IO

-- | Asks for the users first and last name and then prints it
printFullName :: IO ()
printFullName = do
    putStr "What is your first name?\n> "
    firstname <- getLine
    putStr "What is your last name?\n> "
    lastname <- getLine
    putStrLn ("Your full name is: " ++ firstname ++ " " ++ lastname)


-- ? Generating even integers

-- | Generates even integers
genEven :: Gen Integer
genEven = elements [n | n <- [1..100], n `mod` 2 == 0]

-- ? Properties of the lookup-Function

-- | Test if lookup return Just v
prop_lookupExists k v m = lookup k ((k,v):m) === Just v 
  where types = (k :: Int, v :: Int)
    
-- | Tests if lookup returns Nothing
prop_lookupNothing = 
    let kvs = [(k, v) | k <- [1..10], v <- [1..10]] in
        lookup 0 kvs === Nothing

-- ? Generating Dice

data Die = Die Int deriving (Eq, Show)
instance Arbitrary Die where 
    arbitrary = genDie

type Dice = [Die]

-- | Generates a die with a random value between 1-6
genDie :: Gen Die
genDie = elements [Die n | n <- [1..6]]

-- | Generates random dice for yahtzee
genYahtzee :: Gen Dice
genYahtzee = vectorOf 5 genDie

-- ? The Robber Language

-- | Translates a string into Robber Language
toRobber :: String -> String
toRobber [] = []
toRobber (c:cs) | isCons c = (c : 'o' : c : []) ++ toRobber cs
                | otherwise = c : toRobber cs

-- | Checks if a character is a consonant
isCons :: Char -> Bool
isCons c = (c `notElem` "aeiouåäö") && (isAlpha c)

-- | Interacts with the user and translates input into Robber Language
translateRobber :: IO ()
translateRobber = do
    putStr "Welcome to the Robber Language translator!\n\nType a sentence:\n> "
    txt <- getLine
    putStrLn ("In Robber language:\n" ++ toRobber txt)

-- ** Basic
-- ? Sum numbers

-- | Sums an amount of integers
sumOfNumbers :: IO ()
sumOfNumbers = undefined

-- ** Challenging

