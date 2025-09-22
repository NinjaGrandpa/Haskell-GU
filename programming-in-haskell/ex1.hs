module Ex1 where
{- 1.7 Exercises -}
-- 1.Give another possible calculation for the result of double (double 2).
{- Answer:
double x = x + x

double (double 2)
= {applying the inner double}
double (2 + 2)
= {applying double}
(2 + 2) + (2 + 2)
= {applying the first +}
4 + (2 + 2)
= {applying the second +}
4 + 4
= {applying the +}
8

-}
-- 2.Show that sum [x] = x for any number x.
{- Answer:
sum [] = 0
sum (n:ns) = n + sum ns
x + 0 = x
0 + x = x

sum [x]
= {applying sum}
x = sum []
= {applying sum}
x + 0
= {applying +}
x
-}
-- 3.Define a function product that produces the product of a list of numbers, and show using your definition that product [2,3,4] = 24.
{- Answer:
myProduct [2,3,4]
= {applying myProduct}
2 * myProduct [3,4]
= {applying myProduct}
2 * (3 * myProduct [4])
= {applying myProduct}
2 * (3 * (4 * myProduct []))
= {applying myProduct}
2 * (3 * (4 * 1))
= {applying inner *}
2 * (3 * 4)
= {applying inner *}
2 * 12
= {applying inner *}
24
-}

myProduct :: (Num a) => [a] -> a
myProduct [] = 1
myProduct (n : ns) = n * myProduct ns

-- 4.How should the definition of the function qsort be modified so that it produces a reverse sorted version of a list?
-- Answer:
rsort [] = []
rsort (x : xs) = rsort larger ++ [x] ++ rsort smaller
  where
    larger = [b | b <- xs, b > x]
    smaller = [a | a <- xs, a <= x]

-- 5.What would be the effect of replacing <= by < in the original definition of qsort? Hint: consider the example qsort [2,2,3,1,1].
{- Answer:
Every element that should have equal order as x doesn't get added to the "smaller" list and is therefore removed

Is adding the "equal" list faster than using <=? All equal values doesn't need matching with x...
-}

numbers = [1, 1, 2, 3, 4, 5, 5, 5, 5, 5, 6, 5, 111, 111, 111, 556, 556, 556, 1114445]

nsort [] = []
nsort (x : xs) = nsort smaller ++ equal ++ [x] ++ nsort larger
  where
    smaller = [a | a <- xs, a < x]
    larger = [b | b <- xs, b > x]
    equal = [c | c <- xs, c == x]

qsort [] = []
qsort (x : xs) = qsort smaller ++ equal ++ [x] ++ qsort larger
  where
    smaller = [a | a <- xs, a < x]
    larger = [b | b <- xs, b > x]
    equal = [c | c <- xs, c == x]

test = nsort numbers == qsort numbers




