module Ch5 where
-- 5. List Comprehension --
-- 5.2 Guards:
find :: (Eq a) => a -> [(a, b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

{- Example:
> find ’b’ [(’a’,1),(’b’,2),(’c’,3),(’b’,4)]

[2,4]-}

-- 5.3 The zip function
{- Example:
> zip [’a’,’b’,’c’] [1,2,3,4]

[(’a’,1),(’b’,2),(’c’,3)] -}
pairs :: [a] -> [(a, a)]
pairs xs = zip xs (tail xs)

{- Example:
> zip [’a’,’b’,’c’] [1,2,3,4]

[(’a’,1),(’b’,2),(’c’,3)] -}




