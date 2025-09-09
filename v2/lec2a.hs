{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use foldr" #-}

import System.Win32 (xBUTTON1)
import Prelude hiding (last, length, reverse, sum)

-- Recursive functions on lists

-- Length
length :: [Int] -> Int
length [] = 0
length (x : xs) = length xs + 1

-- sum all integers in a list
sum :: [Int] -> Int
sum [] = 0
sum (x : xs) = x + sum xs

-- Get the last element from a list. Easy access to head and tail

last :: [Int] -> Int
last [x] = x
last (x : xs) = last xs

-- reverse a list
reverse :: [Int] -> [Int]
reverse [] = []
reverse (x : xs) = reverse xs ++ [x]