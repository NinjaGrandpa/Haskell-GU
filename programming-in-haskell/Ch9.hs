-- ! Programming in Haskell, Chapter 9 - The Countdown Problem ----

module Ch9 where

-- * 9.2 Arithmetic operators

data Op = Add | Sub | Mul | Div

instance Show Op where 
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"

valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

-- * 9.3 Numeric expressions
-- Type for numeric expression should either be an integer or 
-- the application of an operator to two argument expression

data Expr = Val Int | App Op Expr Expr

instance Show Expr where 
    show (Val n) = show n
    show (App op l r) = brak l ++ show op ++ brak r
      where
        brak (Val n ) = show n
        brak e        = "(" ++ show e ++ ")"

-- > show (App Add (Val 1) (App Mul (Val 2) (Val 3)))
-- "1+(2*3)”

-- | Return the list of values in an expression
values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r

-- | Returns the overall value of an expression
eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App op l r) = [apply op x y | x <- eval l,
                                    y <- eval r,
                                    valid op x y]

-- singleton list means success, empty lis denotes failure
-- > eval (App Add (Val 2) (Val 3))
-- [5]

-- > eval (App Sub (Val 2) (Val 3))
-- []

-- * 9.4 Combinatorial functions

-- | Returns all subsequences of a list
subs :: [a] -> [[a]]
subs [] = [[]] 
subs (x:xs) = yss ++ map (x:) yss
              where yss = subs xs

-- | Returns all possible ways of inserting a new element into a list
interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)


-- | Returns all permutations of a list
perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

-- For example:

-- > subs [1,2,3]
-- [[],[3],[2],[2,3],[1],[1,3],[1,2],[1,2,3]]

-- > interleave 1 [2,3,4]
-- [[1,2,3,4],[2,1,3,4],[2,3,1,4],[2,3,4,1]]

-- > perms [1,2,3]
-- [[1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]]

-- | Returns all choices from a list
choices :: [a] -> [[a]]
choices = concat . map perms . subs

-- For example:

-- > choices [1,2,3]
-- [[],[3],[2],[2,3],[3,2],[1],[1,3],[3,1],[1,2],[2,1],
-- [1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]]

-- * Formalising the problem

-- | Formalises what it means to solve an instance of the countdown problem
solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = 
    elem (values e) (choices ns) && eval e == [n]

-- For example: 
-- > solution e [1,3,7,10,25,50] 765
-- True

-- * 9.6 Brute force solution

-- | Returns all possible ways of spliting a list into two non-empty lists
split :: [a] -> [([a], [a])]
split [] = []   
split [_] = []
split (x:xs) = ([x], xs) : [(x:ls, rs) | (ls,rs) <- split xs]

-- For example:
-- > split [1..4]
-- [([1],[2,3,4]),([1,2],[3,4]),([1,2,3],[4])]

-- | Returns all possible expressions whose list of values is precisely a given list
exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls, rs) <- split ns,
                l        <- exprs ls,
                r        <- exprs rs,
                e        <- combine l r]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

ops :: [Op]
ops = [Add, Sub, Mul, Div]

solutions :: [Int] -> Int -> [Expr]
solutions ns n =
    [e | ns' <- choices ns, e <- exprs ns', eval e == [n]] 

main :: IO ()
main = print (solutions [1,3,7,10,25,50] 765)

-- > ghc -O2 .\Ch9.hs