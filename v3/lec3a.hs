module Lec3a where
-- ! Lecture 3A --

-- ? Operators and sections

{- Operators are just functions! They can be defined an used in an infx or
prefix manner. We need to write parentheses around the operator when we use
it in a prefix application. -}

mul, mul' :: Int -> Int -> Int
mul x y = x * y
mul' x y = (*) x y

{- 
-- * Addition infix
> :i (+)
...
infixl 6 +
> 6 + 5 + 4
15
-- * What it actually means --
> (6 + 5) + 4 
15
 -}

-- * ":"-infix
{- 
> :i (:)
...
infixr 5 :

> 3 : 4 : 5 : []
[3,4,5]
-- * What it actually means--
> 3 : (4 : (5 : []))
-}

-- * (*)-infix
{- 
> :i (*)
...
infixl 7 *
-}

-- * Infix priority/weight
-- (+) = infixl 6
-- (*) = infixl 6

{-
3 + 5 * 8
= { Performing multiplication } -- * Multiplication has a higher weight than addition (7 > 6)
3 + 40
= { Performing addition }
43
 -}

-- * Define the exclusive or (xor) as an operator
(><) :: Bool -> Bool -> Bool
True >< False = True -- Can be defined infix
False >< True = True
_ >< _ = False

infixr 4 >< -- Should bind stronger than && and ||
-- (&&) = infixr 3
-- (||) = infixr 2

-- Calculates to infixl 9 if not defined
-- 0 is the lowest weight, ex. infixr 0 $

-- * Define an operator that adds a pair of elements to a list
(|>) :: (a, a) -> [a] -> [a]
(x, y) |> ys = x : y : ys

infixr 5 |>
-- ghci> (1,2) |> [5,6]

{- 
ghci> (1,2) |> (3,4) |> [5,6]
error
ghci> ((1,2) |> (3,4)) |> [5,6] -- * What it actually says
-- infixl 9 is the standard
infixr 5 |>
(1,2) |> (3,4) |> [5,6]

[1,2,3,4,5,6]
-}

{- 
(1,2) |> [5,6]
= { Apply def }
1 : 2 : [5, 6]
= { Evaluate list cons }
1 : (2 : ([5, 6]))
= { Evaluate list cons }
1 : [2,5,6]
= { Evaluate list cons }
[1,2,5,6]
---
(1,2) |> (3,4) |> [5,6]
-}

-- ? Anonymous functions: lambda expressions
sq :: Int -> Int
sq = \x -> x * x

answer :: Int
answer = 42

list :: [Int]
list = [1, 2, 4, answer]

b :: Bool
b = False

-- Lambda expression with multiple arguments
addl = \x y -> x + y

-- The add* are all the same
add2, add1, add0 :: Int -> Int -> Int
add2 x y = x + y -- variables
add1 x = \y -> x + y -- lambda-abstraction
add0 = \x -> (\y -> x + y) -- application (N M)

-- ? Currying: functions return functions

-- Haskell has only functions with one argument! a -> b
-- but the arguments themselves can be functions

-- Haskell is based on the lambda calculus. Lambda terms can be built from*
--  * variables: x, y, z etc
--  * lambda-abstraction: \x -> M where M is a lambda term, bind the variable
--  * application (N M) where N and M are lambda terms

-- and uses the following reduction operations:
--  * alpha-renaming: renaming bound variables
--  * beta-reduction: (\x -> M) N => M[x := N], substitution of x in M
--  * eta-reduction: \x -> f x => f, with x not free in f

-- Functions are returning functions, one can say that the function type (->)
-- associates to the right.

-- An example, f x y z = x + y + <:
f :: Int -> (Int -> (Int -> Int)) -- same as: Int -> Int -> (Int -> Int)
f = \x -> (\y -> (\z -> x + y + z)) -- same as: \x -> \y -> \z -> x + y + z

-- function applications binds the strongest

-- The dual: application associates to the left:
app_f = ((f 1) 2) 3

--    = (f 1) 2 3 -- the same
--    = f 1 2 3 -- again, the same

-- Applying add0 to an argument returns a function! Which can be applied to
-- another argument.
{-
add0 2 3
= { function app binds strongest }
(add0 2) 3
= { fun def }
((\x -> (\y -> x + y)) 2) 3
= { beta reduce }
(\y -> 2 + y) 3
= { beta reduce }
2 + 3
=
5
-}
-- ? Partial application

-- Applying a function to too few arguments does not result in a type error,
-- but it with *return* a function
inc :: Int -> Int
inc = add0 1

-- For example, partial application of take
mamomi :: [String] -- partial app take
mamomi = [takeTwo xs | xs <- ["Max", "Molly", "Mira"]]
  where
    takeTwo = take 2

-- ? Eta reduction (and expansion)

-- Pair a list of elements with their index. The function can be eta-reduced
withIndex, withIndex', withIndex'' :: [a] -> [(Int, a)]
withIndex xs = zip [0 ..] xs
withIndex' = \xs -> (zip [0 ..]) xs -- eta-reduced
withIndex'' = zip [0 ..] -- further eta-reduced

-- Eta-reduce the rev function
rev, rev', rev'' :: [a] -> [a]
rev xs = go [] xs
  where
    go acc [] = acc
    go acc (x : xs) = go (x : acc) xs
rev' = \xs -> (go []) xs
  where
    go acc [] = acc
    go acc (x : xs) = go (x : acc) xs
rev'' = go []
  where
    go acc [] = acc
    go acc (x : xs) = go (x : acc) xs

-- Formally: \x -> g x =={eta}==> f

-- ? Sections (partial application of infix operators)

-- We can partially apply an operator. This is also called an operator section.
-- The plus operator expects two arguments (operands), but when only giving it
-- one argument it returns a function, which expects another.

{-
ghci> f x = x + 1

ghci> map f [1,2,3]
[2,3,4]

ghci> map (\x -> x + 1) [1,2,3]
[2,3,4]

> map (+ 1) [1,2,3]
[2,3,4]
-}

{-
> timesTwo = (*) 2
> timesTwo 4
8
 -}

-- * Eta-expand & Beta-reduction

plusOne :: Int -> Int
plusOne = (1 +)

{-
plusOne 41
= { def plusOne }
(1 +) 41
= { eta expand }
(\x -> 1 + x) 41
= { beta reduce }
1 + 41
= { perform addition }
42
-}

-- * Triple: multiply all elements in a list with three

triple :: [Int] -> [Int]
triple xs = let f = (3 *) in [f x | x <- xs]




