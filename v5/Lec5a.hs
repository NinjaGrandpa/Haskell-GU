-- ! Lecture 5A: recursive data types ----

module Lec5a where

import Prelude hiding (sum)
import Test.QuickCheck

-- * Natural numbers (Peano numbers) -------------------------------------------

-- | A natural number is either zero, or the successor of a natural number
data Nat = Zero | Succ Nat

instance Show Nat where
    show n = show (fromNat n) ++ "n"

one, two, three :: Nat
one = Succ Zero
two = Succ (Succ Zero)
three = Succ two

toNat :: Int -> Nat
toNat 0 = Zero
toNat n | n > 0 = Succ (toNat (n-1))
        | otherwise = error "No negative naturals!"

-- toNat with foldr
toNat' :: Int -> Nat
-- toNat' = foldr (\f n -> f n) Zero (replicate 13 Zero)
toNat' = foldr ($) Zero . flip replicate Succ

-- Exercise fromNat
fromNat :: Nat -> Int
fromNat Zero = 0
fromNat (Succ n) = 1 + fromNat n

-- | We can not take a negative number of elements from a list
takeNat :: Nat -> [a] -> [a]
takeNat (Succ n) (x:xs) = x : takeNat n xs
takeNat _        _      = []

-- | Exercise: The length of a list is a natural number (it can not be negative)
length' :: [a] -> Nat
length' = undefined

-- Add two natural numbers
add :: Nat -> Nat -> Nat
add a Zero = a
add a (Succ b) = Succ (a `add`b)

-- mul :: Nat -> Nat -> Nat

-- Exercise: define substraction for natural numbers
-- sub :: Nat -> Nat -> Nat

-- instance Arbitrary Nat where

-- prop_commutative :: Nat -> Nat -> Bool

-- prop_associative :: Nat -> Nat -> Nat -> Bool

instance Num Nat where
    (+) = add
    -- (*) = mul
    fromInteger = toNat . fromInteger -- How does it work?


-- * Introducing recursive data types ------------------------------------------

-- Simple recursive data type for lists with integer elements
data ListInt = EmptyList | Add Int ListInt deriving Show

-- Example lists
xs :: ListInt
xs = EmptyList
ys = Add 3 xs
zs = Add 3 (Add 2 (Add 1 EmptyList))

-- Calculating the length of a integer list, i.e., the number of elements
len :: ListInt -> Int
len EmptyList = 0
len (Add x xs) = 1 + len xs

-- data type for list with strings
data ListStr = EmptyS | AddS String ListStr deriving Show
-- AddS "Max" (AddS "Jenslöv" EmptyS)

-- Generalize: introduce type variable
data List a = Nil | Cons a (List a) deriving Show

-- Example lists with integers and strings
xs' :: List Int
xs' = Cons 42 (Cons 2 (Cons 3 Nil))

ys' :: List String
ys' = Cons "max" (Cons "jenslöv" Nil)

-- Recursive functions on lists --

-- Summing all elements in a polymorphic list
sum :: Num a => List a -> a
sum Nil         = 0
sum (Cons x xs) = x + sum xs

-- Showing lists
showList :: Show a => [a] -> String
showList = undefined

-- Folding over a Hand
foldList :: (a -> b -> b) -> b -> List a -> b
foldList op b Nil = b
foldList op b (Cons x xs) = x `op` foldList op b xs

size :: List a -> Int
size = foldList (\x n -> n + 1) 0

-- | ???
(|>) :: a -> List a -> List a
x |> xs = undefined

fromList :: List a -> [a]
fromList = foldList (:) []

-- exercise, with foldr, toList [1,2,3] == Cons 1 (Cons 2 (Cons 3 nil))
toList :: [a] -> List a
toList xs = foldr (\x -> Cons x) Nil xs

-- back to presentation

--------------------------------------------------------------------------------
-- * Tree-shaped data types (branching data structures)

{- 
? Linear recursive data types
* The values in these types are all linera sequences of things

data Hand = Empty | add Card Hand
...

? Branching data structures
* How would we create a data type for flowcharts (action diagrams)?

Do you have problems ?
    > YES
        > Can you do anything about them?
            > YES
                > Then don't worry
                    > Done
            > NO
            > Then don't worry
                    > Done
    > NO 
        > Then don't worry
            > Done!
 -}

-- | Action Diagrams
data Diagram
    = Question String Diagram Diagram
    | Action String Diagram
    | Done
    deriving Show    

isSunny, park, work :: Diagram
isSunny = Question "Is it sunny outside?" park work
park    = Action "Go to the park! And write some Haskell code!" Done
work    = Action "Write some Haskell code!" Done

-- * "play" a diagram
play :: Diagram -> IO ()
play d = case d of
    Question question yes no -> do
        putStrLn question
        putStr "[y/n] >"
        answer <- getLine
        case answer of
            "y" -> play yes
            "n" -> play no
            _   -> do 
                putStrLn "Try again!"
                play d
        
    Action action cont -> do
        putStrLn action
        play cont
    
    Done -> return ()

-- Pictionary flowchart
-- pictionary :: Diagram
-- pictionary = Action "Draw a picture" guessedIt
-- guessedIt  = Question "Did they guess it?" win point
-- win        = Action "You win!" Done
-- point      = Action "Point repeatedly to the same picture!" guessedIt


-- Menti!

-- * Binary trees --------------------------------------------------------------

-- | Binary trees with numbers in the nodes

-- leaf :: a -> Tree a

-- t :: Tree Int
-- t = Node (Node (leaf 2) 3 (leaf 4)) 6 (Node (leaf 7) 8 (leaf 9)) 

-- | The sum of the numbers in a binary tree
-- sumTree :: Num a => Tree a -> a

-- | The number of elements in a tree
-- sizeTree :: Tree a -> Int

-- | The height of a binary tree
-- height :: Tree a -> Int

-- * Advanced stuff with trees -------------------------------------------------

-- foldTree :: (b -> a -> b -> b) -> b -> Tree a -> b

-- toList :: Tree a -> [a]


-- mapTree :: (a -> b) -> Tree a -> Tree b

