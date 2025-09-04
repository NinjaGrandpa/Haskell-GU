-- Föreläsning 3 : 2025-09-04

-- Guards equations:

-- * Alternative to if-then-else

-- * otherwise case

-- Return the maximum of two values, now using guards
max x y
  | x < y = y
  | x > x = x
  | otherwise = y

-- Nestled

bla x =
  if x > 0
    then
      if x > 5
        then 4
        else 5
    else 6

-- Collatz number
collatz x
  | even x = x `div` 2
  | otherwise = 3 * x + 1

-- Use the command "it" in terminal to use the latest result
-- Command "takeWhile (/=1) $ iterate collatz 12"