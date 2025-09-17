-- ! Lecture 3B: IO instructions and test data generation --

{- 
? Recap:
 * Defining operator
 * Operator fixity and binding precedence
 * Currying
 * Lambda expressions
 * Partial application
    * Operator Sections
 -}

 {- 
 ? Today:
    * Input/output (IO) in Haskell
        * Printing an getting input from users
        * Reading to and writing from files
        * De-Notation
    * Generating test data using QuickCheck
      * Generator combinators
      * Do-Notation
      * Type class Arbitrary
      * Testing properties

  -}

  ---------------------------------------------------------------------------------------------------------------------------------
{-

! Input/Output(IO)

? Introduction
? The Problem
  * Haskell programs are pure mathematical functions (no side effects)
  * However, reading from the keyboard and writing to the screen are side effects:

? Pure Functions
 * What is a function?
    * Single result for each input
    * A function given a specific input(s) produces the same results every time

? How would you do that?
* n-sided die:
    die :: Int -> Int
* Read from a file:
    type FileName = String
    readFromFile :: FileName -> String
* A the contents of a file can change even given the same input (the file)

? Haskell Instructions
* Solved this dilemma by introducing special type instructions (sometimes calld actions or commands)
* IO Integer (for example) is the type of instructions for producing an integer

? What is the type of 'writeFile'
ghci> :t writeFile
writeFile :: FilePath -> String -> IO ()
* When GHCI gets an expression of type IO, it executes the instructions
* Note: The function 'writeFile' does not write the file. It only computes the instruction to write.
* The latest expression called, the order of statements is important when using IO

? The type ()
* The type () is called the unit type
* It only has one value, namely ()
* We cansee () as the "empty tuple"
* Called void in many imperative languages
* It means that there is no interesting result
data () = ()
-}

import Test.QuickCheck

-- Hello World
hello :: IO ()
hello = putStrLn "Hello, World!"

-- Greeting! Ask for a name and greet.
-- Do-Notation, evaluates in order

greeting :: IO ()
greeting = do
    putStr "What is your name?\n> "
    name <- getLine -- Not name = getLine, name <- getLine binds the result to name instead of itself being x
    putStrLn ("Hello " ++ name ++ "!")

-- Make a backup of a file
backup :: FilePath -> IO ()
backup file = do
    txt <- readFile file
    writeFile (file ++ ".bak") txt

-- Whats The Difference?
-- whatsTheDifference :: IO ()
-- whatsTheDifference = do
--     putStr "Give me two numbers separated by a space:\n> "
--     txt <- getLine
--     putStrLn ("The diff is: " ++ show (read (head wrds) + read (last wrds)))
--     where wrds t = words txt


-- Comput the number of lines in a file
lengthFile :: FilePath -> IO Int
lengthFile file = do
    txt <- readFile file
    return (length (lines txt)) -- 'return' returns a monad

-- More modular version of whatsTheDifference
askForNumber :: IO Int
askForNumber = do
    putStr "Give a number:\n> "
    n <- readLn
    return n

calcDiff :: IO Int
calcDiff = do
    x <- askForNumber
    y <- askForNumber
    return (x-y)

whatsTheDifference' :: IO ()
whatsTheDifference' = do
    diff <- calcDiff
    putStrLn ("The difference is " ++ show diff)

-- | translate a sentence to the "pirate language" (rövarspråket)
-- Example: rövarspråket "bar" == "bobaror"
rövarspråket :: String -> String
rövarspråket txt = undefined
    where v = ["e", "u", "o", "å", "a", "ö", "ä"]

runRövarspråket :: IO ()
runRövarspråket = undefined

{-
? Generating 
* Generate random inputs is hard in Haskell because of pure functions
* The library QuickCheck uses IO to randomize inputs
 -}

data Suit = Hearts | Spades | Diamonds | Clubs       deriving (Eq, Read)
data Rank = Numeric Int | Jack | Queen | King | Ace  deriving (Eq, Read)
data Card = Card Rank Suit                           deriving (Eq, Read)
type Hand = [Card]

instance Show Suit where
  show Hearts   = "H"
  show Spades   = "S"
  show Diamonds = "D"
  show Clubs    = "C"

instance Show Rank where
  show (Numeric n) = show n
  show Jack        = "J"
  show Queen       = "Q"
  show King        = "K"
  show Ace         = "A"

instance Show Card where
  show (Card r s) = show r ++ show s

validRank :: Rank -> Bool
validRank (Numeric n) = 2 <= n && n <= 100
validRank _           = True

rankBeats :: Rank -> Rank -> Bool
rankBeats _ Ace = False
rankBeats Ace _ = True
rankBeats _ King = False
rankBeats King _ = True
rankBeats _ Queen = True
rankBeats Queen _ = False
rankBeats _ Jack = False
rankBeats Jack _ = True
rankBeats (Numeric x) (Numeric y) = x > y

prop_beats :: Rank -> Rank -> Bool
prop_beats x y
  | x == y    = not (x `rankBeats` y)
  | otherwise = x `rankBeats` y /= y `rankBeats` x

-- * Generators for Rank and Suit: elements, sample, generate ------------------

-- | Generate a random Rank, version 1

allRanks :: [Rank]
allRanks = [Numeric n | n <- [2 .. 100]] ++ [Jack, Queen, King, Ace]

genRank :: Gen Rank
genRank = elements allRanks

-- Test the generated ranks, introduce collect and Property data type
-- use forAll to choose a specific generator

prop_rank :: Property
prop_rank = forAll genRank validRank

-- Addition is cummutative
prop_add :: Int -> Int -> Bool
prop_add x y = x + y == y + x

-- type Arbitrary, can generate randomized values

-- | Make Rank an instance of Arbitrary
instance Arbitrary Rank where
    arbitrary = genRank

-- sample (arbitrary :: Gen <type>), Generates 10 random values based on a type
-- sample runs using a generator of typ Gen <Type>
-- * Change distribution of test data

genRoyal, genNumeric :: Gen Rank
genRoyal = elements [Jack, Queen, King, Ace]
genNumeric = elements [Numeric n |n <- [2..100]]

-- Using frequency to get an even distribution of ranks
genRankFreq :: Gen Rank
genRankFreq = frequency[(3, genRoyal), (1, genNumeric)]

genSuit :: Gen Suit
genSuit = elements [Hearts, Diamonds, Clubs, Spades]

-- | Make Suit an instance of Arbitrary
instance Arbitrary Suit where
    arbitrary = genSuit

-- | Generating random cards
genCard :: Gen Card
genCard = do
    r <- genRankFreq
    s <- genSuit
    return (Card r s)

-- run sample genCard

-- The command ":browse" is used to prowse a library 

-- | Generate a random hand with more control over size

-- "vectorOf" creates a list of size x, can be used 
-- vectorOf 52 genCard

genNonNegative :: Gen Integer
genNonNegative = do
    n <- arbitrary
    return (abs n)