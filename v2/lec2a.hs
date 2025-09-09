{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use foldr" #-}
import Prelude hiding (length, sum)

-- Recursive functions on lists

-- Length
length :: [Int] -> Int
length [] = 0
length (x : xs) = length xs + 1

-- sum all integers in a list
sum :: [Int] -> Int
sum [] = 0
sum (x : xs) = x + sum xs
