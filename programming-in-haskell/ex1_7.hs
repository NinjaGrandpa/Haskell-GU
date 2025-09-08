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

-- 4.How should the definition of the function qsort be modified so that it produces a reverse sorted version of a list?

-- 5.What would be the effect of replacing <= by < in the original definition of qsort? Hint: consider the example qsort [2,2,3,1,1].
