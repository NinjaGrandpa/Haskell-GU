-- Föreläsning 3 : 2025-09-04 --

-- Guards equations: --

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

-- Recursion --
power n k | k < 0 = error "power: negative exponent"
power n 0 = 1
power n k = n * power n (k - 1)

-- Base Case stops the recursion aka. x 0 = 1
-- Factorial
fac 0 = 1
fac n = n * fac (n - 1)

-- Pattern Matching

-- Fibonacci numbers
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

{--
fib 3 = fib 2 + fib 1 = (fib 1 + fib 0) + fib
--}

-- Types --
-- Strongly typed language
a :: Int
a = 42

b :: Bool
b = True

c :: Char
c = 'C'

d :: String
d = "String"

e :: Double
e = 1.0

f :: Float
f = 0.5

g :: Integer
g = 2

-- Function Types:
plusOne :: Int -> Int
plusOne x = x + 1

multiply :: Int -> Int -> Int
multiply x y = x * y

greet :: String -> String
greet name = "Tjenare " ++ name ++ "!"