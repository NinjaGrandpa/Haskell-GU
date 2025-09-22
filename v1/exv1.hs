module Exv1 where
import Data.Void (vacuous)

-- Exercices - Week 1
-- ghci> 4*(8-5)+3
-- 15
-- ghci> 3.5/(3+4)*4.5
-- 2.25
-- ghci> sin 1.57
-- 0.9999996829318346

-- 1. The first of the expressions contains a multiplication-,  addition-,  and a subtraction operator.
-- In which order are these operators applied? Which numbers are multiplied, added, and subtracted?
{--
4*(8-5)+3
8-5
4*3
12+3 = 15
--}
-- 2. What will the result be if we take away the parentheses in the first expression, i.e. 4*8-5+3?
{--
4*8 = 32
32-5 = 27
27+3 = 30
--}
-- 3.In which order are the operators applied in the second expression?
-- Multiplication, Subtraction, Addition

-- 4. The sell rate for the English pound was 12.7775 SEK on the 20th of October 2008.
-- How many pounds did one get for 1000 SEK that day?

pounds kr = kr / 12.7775

-- Anwswer: 78.2625709254549 pounds

-- 5. In some countries people use the Fahrenheit temperature scale. 0° Celsius (C) corresponds to 32° Fahrenheit (F),
-- and an increase of 5° C corresponds to an increase of 9° F. How many Fahrenheit degrees is 28° C?

-- Answer 82.4 F

-- 6. Define a function that converts temperatures in °C to °F.
-- Add this function to ex1.hs. Load the file and test your function.

toFahrenheit c = (c / 5) * 9 + 32

-- 7. We will now introduce a new kind of error.
-- What happens if one writes pount 10 (note the spelling error)? What happens if one writes: pounds kr?

{-- Answer:
<interactive>:10:1: error: [GHC-88464]
    Variable not in scope: pount :: t0 -> t

<interactive>:11:8: error: [GHC-88464]
    Variable not in scope: kr
    Suggested fix: Perhaps use `or' (imported from Prelude)

--}

priceA v
  | v <= 10 = 3.5 * v
  | v > 10 = 5 + 3 * v

q = [9.5, 10.5, 11.5]

-- 8.  Change the last of the definitions to:
--    | v > 11  = 5+3*v
-- What is the result of applying the function to 9.5, 10.5 and 11.5, respectively?
priceB v
  | v <= 10 = 3.5 * v
  | v > 11 = 5 + 3 * v

{--
Answer:
    33.25
    *** Exception: v1\Exv1.hs:(66,1)-(68,22): Non-exhaustive patterns in function priceB
    39.5
--}

-- 9.  Change the function to:
-- price v
--    | v <= 11 = 3.5*v
--    | v > 10  = 5+3*v
-- What is the now result of applying the function to 9.5, 10.5 and 11.5, respectively?

priceC v
  | v <= 11 = 3.5 * v
  | v > 10 = 5 + 3 * v

{-- Answer:
    33.25
    36.75
    39.5
--}

priceD v
  | v <= 10 = 3.5 * v
  | otherwise = 5 + 3 * v

-- 10. The sale of potatoes has been so successful that the shop now can offer a price of 2.50 SEK/kilo for quantities exceeding 20 KG
-- (with the same prices for other amounts). Change the function to to calculate the correct price.

priceE v
  | v <= 10 = 3.5 * v
  | otherwise = 5 + 2.5 * v

-- 11.  Try to evaluate the last expression without parentheses. Can you explain the result?
{--
ghci> price 7.5
26.25
ghci> price 11
38.5
ghci> price (4+8)
41.0
ghci> price 4+8

<interactive>:20:1: error: [GHC-88464]
    Variable not in scope: price :: t0 -> a
    Suggested fix:
      Perhaps use one of these:
        `priceA' (line 58), `priceB' (line 67), `priceC' (line 84)
--}

{-- Answer:
It first calculates the price of 4 kg of potatoes and the adds 8 kr to the final price instead of calculating 12 kg of potatoes
--}

-- Functions with more than one argument

-- 12. Define a function that gives the average of three numbers. Load in the function in GHCi and test it.
-- Answer:
average x y z = (x + y + z) / 3

-- Operators and functions with two arguments
-- 13. What is 5 `mod` 0?
{-- Answer:
ghci> 5 `mod` 0
\*** Exception: divide by zero
--}
-- 14. Is 107139224 divisible by 11731?
-- Answer: yes, 9133.000085244224, but it's no longer an integer

-- 15. Define a function that given a year gives the number of days in that year.
-- Used the simplified rule that year numbers divisible by four are leap years (years with 366 days).
-- You will have to use relational operator == which evaluates to True if its arguments are equal.

daysInYear y
  | y `mod` 4 == 0 && y `mod` 100 == 0 && y `mod` 400 == 0 = 366
  | y `mod` 4 == 0 && y `mod` 100 == 0 = 365
  | y `mod` 4 == 0 = 366
  | otherwise = 365




