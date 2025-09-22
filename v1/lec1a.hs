module Lec1a where
-- :t command show the type of any expression
-- The "::"-symbol indicates a type signature, "is of type"

-- Prelude> :type True
-- True :: Bool
-- Prelude> :type False
-- False :: Bool
-- Prelude> :t (3 < 5)
-- (3 < 5) :: Bool

-- chr :: Int -> Char, decodes int to char
-- ord :: Char -> Int, encodes a char to a numeric code
-- use the :module or :m Data.Char to import the module where the commands are defined

-- Functions with more than one argument

-- Write the type signature above the corresponding function definition
xor :: Bool -> Bool -> Bool
xor p q = (p || q) && not (p && q)

-- Type inference
isL :: Char -> Bool
isL c = c == 'l'

-- Both arguments of "==" must have the same type
-- Can write type signatures of the same type in one signature separated by commas
firstName, lastName :: String
firstName = "Max"
lastName = "Jenslov"

-------------------------------------------------------------------------------------------------------------

-- Recursion
-- A recursive function is a function that has the ability to invoke itself

-- Factorial function
-- Takes a single non-negative integer as an argument and finds all the positive integers less than or equal to "n", and multiplies them all together
-- Factorial of 6 (denoted as 6!) is 1*2*3*4*5*6 = 720

-- Factorial of 6 = 6 × 5 × 4 × 3 × 2 × 1
-- Factorial of 5 = 5 × 4 × 3 × 2 × 1
-- 6! includes 5! and is in fact just 6 * 5!

factorial 0 = 1
factorial n = n * factorial (n - 1)

-- Why does the function return when n = 0?

-- List multiple function definitions starting with the most specific and proceeding to the most general

-- Double Factorial
-- The double factorial of number n is the product of all the integers 1 up to n that have the same parity as n
-- doubleFactorial 8 -> 8 * 6 * 4 * 2 = 384
-- doubleFactorial 7 -> 7 * 5 * 3 * 1 = 105
doubleFactorial 0 = 1
doubleFactorial 1 = 1
doubleFactorial n = n * doubleFactorial (n - 2)

-- Loops, recursion and accumulating parameters
-- Imperative languages use loops in the same contexts where Haskell uses recursion
fact n = go n 1
  where
    go n res
      | n > 1 = go (n - 1) (res * n)
      | otherwise = res

-- where
-- go
-- res

-- Guarded equations
-- Used to make definitions with multiple conditions
-- The catch all condition otherwis is defined in the Prelud by "otherwise = True"

yesNo n
  | n = "Yes"
  | not n = "No"

-- Pattern Matching



