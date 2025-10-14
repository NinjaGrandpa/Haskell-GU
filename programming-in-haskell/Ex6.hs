-- ! 6.8 Exercises ----

module Ex6 where

import Prelude hiding ((^), and, concat, replicate, (!!), elem) 

{- 
* 1.How does the recursive version of the factorial function behave if applied to a negative argument, 
* such as (-1)* Modify the definition to prohibit negative arguments by adding a guard to the recursive case. -}
-- ? Answer:

fac :: Int -> Int
fac 0 = 1
fac n | n < 0 = 0
      | otherwise =  n * fac (n-1)

{- Negative argument
fac (-1)
= {applying fac}
(-1) * fac (-2)
= {applying fac}
(-1) * ((-2) * fac (-3))
...
-}

{- 
* 2.Define a recursive function sumdown :: Int -> Int that returns the sum of the non-negative integers from a given value down to zero. 
For example:
sumdown 3 should return the result 3+2+1+0 = 6. -}

sumdown :: Int -> Int
sumdown 0 = 0
sumdown n | n > 0 = n + sumdown (n-1)

{- 
* 3.Define the exponentiation operator ^ for non-negative integers using the same pattern of recursion as the multiplication operator *, 
* and show how the expression 2 ^ 3 is evaluated using your definition. -}

(^) :: Int -> Int -> Int
b ^ 0 = 1
b ^ n | n > 0 = b * (b ^ (n-1))
      | otherwise = error "Negative exponent"

{- 
2 ^ 3
= { applying ^ }
2 * (2 ^ 2)
= { applying ^ }
2 * (2 * (2 ^ 1))
= { applying ^ }
2 * (2 * (2 * (2 ^ 0) ))
= { applying ^ }
2 * (2 * (2 * 1 ))
= { applying * }
8
-}

{- 
* 4.Define a recursive function euclid :: Int -> Int -> Int that implements Euclid’s algorithm for calculating the greatest common divisor of two non-negative integers: 
* if the two numbers are equal, this number is the result; otherwise, the smaller number is subtracted from the larger, 
* and the same process is then repeated. 
For example:
> euclid 6 27
3 
-}
-- ? Answer:
euclid :: Int -> Int -> Int
euclid n 0 = n
euclid 0 m = m
euclid n m | n < 0 || m < 0 = error "Enter non-negative numbers"
           | n == m = n
           | otherwise = euclid ((max n m) - (min n m)) (min n m)

{- 
euclid 45 10
= { applying euclid }
euclid 35 10
= { applying euclid }
euclid 25 10
euclid 15 10
euclid 5 10
euclid 5 5
5
-}

-- * 5.Using the recursive definitions given in this chapter, show how length [1,2,3], drop 3 [1,2,3,4,5], and init [1,2,3] are evaluated.
-- ? Answer:
{- 
length :: [a] -> Int
length [] = 0
length (_:xs) = 1 + length xs

length [1,2,3]
= { applying length }
1 + length [2,3]
= { applying length }
1 + (1 + length [3])
= { applying length }
1 + (1 + (1 + length []))
= { applying length }
1 + (1 + (1 + 0))
= { applying + }
3

-------------------------------
drop :: Int -> [a] -> [a]
drop 0 xs = xs
drop _ [] = []
drop n (_: xs) = drop (n-1) xs

drop 3 [1,2,3,4,5]
= { applying drop }
drop 2 [2,3,4,5]
= { applying drop }
drop 1 [3,4,5]
= { applying drop }
drop 0 [4,5]
= { applying drop }
[4,5]

-------------------------------
init :: [a] -> [a]
init [_]= []
init (x:xs) = x : init xs

init [1,2,3]
= { applying init }
1 : init [2,3]
= { applying init }
1 : (2 : init [3])
= { applying init }
1 : (2 : [])
= { applying : }
[1,2]

 -}

{- 
* 6.Without looking at the definitions from the standard prelude, define the following library functions on lists using recursion.
-Note: most of these functions are defined in the prelude using other library functions rather than using explicit recursion, 
and are generic functions rather than being specific to the type of lists. -}

-- * a.Decide if all logical values in a list are True:
and :: [Bool] -> Bool
and [] = True
and (x:xs) = x && and xs

-- * b.Concatenate a list of lists:
concat :: [[a]] -> [a]
concat [] = []
concat (xs:xss) = xs ++ concat xss

-- ** c.Produce a list with n identical elements:
replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n x = x : replicate (n-1) x

-- ** d.Select the nth element of a list:
(!!) :: [a] -> Int -> a
[] !! _ = error "index too large"
(x:xs) !! 0 = x
(x:xs) !! n = xs !! (n-1)

-- ** e.Decide if a value is an element of a list:
elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem x (x':xs) = x == x' || elem x xs -- lazy evaluation

{- 
* 7.Define a recursive function merge :: Ord a => [a] -> [a] -> [a] that merges two sorted lists to give a single sorted list. 
For example:
    > merge [2,5,6] [1,3,4]
    [1,2,3,4,5,6]

Note: your definition should not use other functions on sorted lists such as insert or isort, but should be defined using explicit recursion. -}

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x > y = y : merge (x:xs) ys
                    | otherwise = x : merge xs (y:ys)

prop_merge = merge [2,5,6] [1,3,4]
{- 
merge [2,5,6] [1,3,4]
1 : merge [2,5,6] [3,4]
1 : (2 : (merge [5,6] [3,4]))
1 : (2 : (3 : (merge [5,6] [4])))
1 : (2 : (3 : (4 : (merge [5,6] []))))
1 : (2 : (3 : (4 : [5, 6])))
[1,2,3,4,5,6]
-}

{-
* 8.Using merge, define a function msort :: Ord a => [a] -> [a] that implements merge sort,
* in which the empty list and singleton lists are already sorted, 
* and any other list is sorted by merging together the two lists that result from sorting the two halves of the list separately.
    Hint: first define a function halve :: [a] -> ([a],[a]) that splits a list into two halves whose lengths differ by at most one. -}

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = 
    let (ys, zs) = halve xs
    in merge (msort ys) (msort zs)


halve :: [a] -> ([a], [a])
halve [] = ([], [])
halve xs = half (length xs `div` 2) xs
  where
    half 0 ys = ([], ys)
    half _ [] = ([], [])
    half n (y:ys) = 
        let (first, second) = half (n-1) ys
        in (y:first, second)

{- 
* 9.Using the five-step process, construct the library functions that:
* a.calculate the sum of a list of numbers;

* b.take a given number of elements from the start of a list;

* c.select the last element of a non-empty list.-}