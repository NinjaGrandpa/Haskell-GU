module Exv4 where

import Test.QuickCheck
import Data.Maybe (isNothing, isJust)
import Data.Char (isAlpha, toLower)
import Data.List (notElem)
import GHC.Stack (HasCallStack)
import Data.List.NonEmpty (NonEmpty)
import System.Directory


-- ! Excercises week 4: IO and testing --

-- * Self-check
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

-- * Basic
-- ? Sum numbers

-- | Sums an amount of integers
sumOfNumbers :: IO ()
sumOfNumbers = do
    putStr "----- Sum Of Numbers -----\nHow many numbers do you want sum?\n> "
    n <- readLn
    ns <- askForNumber n
    putStrLn ("The sum of the numbers are: " ++ (show ns))

-- | Asks the user for n amount of numbers and returns the sum
askForNumber :: Int -> IO Int
askForNumber 0 = return 0
askForNumber n = do
    putStr "Write a number:\n> "
    m <- readLn
    ns <- askForNumber (n-1)
    return (m + ns)

-- ? Properties of the prefixOf function

-- | Checks if a list is a prefix of another list
prefixOf :: Eq a => [a] -> [a] -> Bool
prefixOf [] _          = True
prefixOf _  []         = False
prefixOf (x:xs) (y:ys) = x == y && prefixOf xs ys

-- | Expresses that taking any number of elements from a string gives alist that is a prefix of s
prop_prefixTake :: String -> Bool
prop_prefixTake s = and [prefixOf (take n s) s | n <- [0..(length s-1)]]

-- | Expresses that a list appended to a previous list, the first list becomes the prefix of the second
prop_prefixAppend :: Eq a => [a] -> [a] -> Bool
prop_prefixAppend xs ys = xs `prefixOf` (xs ++ ys)

-- * Advanced questions
-- ? The Number Guessing Game

-- list of guesses, guess = [(Int, String)]
-- Using list comprehension filter so that new guess is passes all previous guesses

type Guess = (Int, String)

game :: IO ()
game = do
    putStrLn "Think of a number between 1 and 100.\nPress Enter to continue..."
    getLine
    (n, _) <- makeGuess []
    putStrLn ("I win!\nThe answer was: " ++ show n)

makeGuess :: [Guess] -> IO Guess
makeGuess gs = do
    let n = calcGuess gs
    putStr ("Is it " ++ (show n) ++ "?\n> ")
    answer <- getLine
    let g = (n, answer)
    if answer == "yes" then
        return g
    else makeGuess (g : gs)


parseAnswer ::  (Eq a, Ord a) => String -> (a -> a -> Bool)
parseAnswer a = case a of
    "higher" -> (>)
    "lower"  -> (<)
    "yes"    -> (==)


calcGuess :: [Guess] -> Int
calcGuess [] = 50
calcGuess guesses = let ns = [n | n <- [1..100], (g, a) <- guesses, (parseAnswer a) n g && g /= n] in
    head $ drop ((length ns) `div` 2 - 1) ns

calcGuess' :: [Guess] -> (Int, [Int])
calcGuess' guesses = let ns = [n | n <- [1..100], (g, a) <- guesses, (parseAnswer a) n g && g /= n] in 
    (head $ drop ((length ns) `div` 2) ns, ns)

gs :: [Guess]
gs = [(50, "lower"), (24, "higher"), (43, "higher")]

-- Is it 50?
-- > lower
-- Is it 24?
-- > higher
-- Is it 43?
-- > higher
-- Is it 55?


-- ? A backup script
-- creates a new directory called "backup",
-- copies all the files in the current directory into the backup directory.

-- getDirectoryContents :: FilePath -> IO [FilePath]
-- getCurrentDirectory :: IO FilePath
-- setCurrentDirectory :: FilePath -> IO ()
-- withCurrentDirectory
-- copyFile

backup :: IO ()
backup = do 
    dir <- getCurrentDirectory
    files <- getDirectoryContents dir
    let bdir = dir ++ "\\backup"
    createDirectoryIfMissing False bdir
    copyFiles dir bdir files
    putStrLn (show files)

copyFiles :: FilePath -> FilePath -> [FilePath] -> IO ()
copyFiles sdir ddir fs = do sequence_ $ map (\f -> copyFile (sdir\\f) (ddir\\f)) fs
 where dir \\ name = dir ++ "\\" ++ name

-- ? Generating Lists


-- ? Generating Ordered Lists


-- ? Hangman




