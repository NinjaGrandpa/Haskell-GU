{- |
Module      : Lec6a
Description : Lecture week 6, Part A, Arithmetic Expressions
Maintainer  : max@jenslov.se

How to model and work with simple expression languages.

- Binary trees
- Arithmetic expressions
  * Modelling
  * Evaluating
  * Pretty printing
  * Generating, taking care of size
-}

module V6.Lec6a where 

import Data.List (intercalate, union)
import Data.Maybe 
import Test.QuickCheck 

-- * Binary trees

-- | Binary trees with numbers in the nodes
data Tree a = Empty | Node (Tree a) a (Tree a) deriving (Eq, Show)

leaf :: a -> Tree a 
leaf x = Node Empty x Empty

t :: Tree Int
t = Node (Node (leaf 2) 3 (leaf 4)) 6 (Node (leaf 7) 8 (leaf 9))

{- 
     6
 3 8
2  4  7  9

      6
  3   -   8
2 - 4   7 - 9
 -}

-- | The number of elements in a tree
sizeTree :: Tree a -> Int
sizeTree t = case t of
    Empty      -> 0
    Node l x r -> sizeTree l + 1 + sizeTree r

-- | The height of a binary tree
height :: Tree a -> Int
height t = case t of
    Empty      -> -1
    Node l _ r -> 1 + max (height l) (height r)
    
-- | Fold over a tree
foldTree :: (a -> b -> b) -> b -> t a -> b
foldTree f b a = undefined

-- | Map a tree
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f tree = case tree of
  Empty      -> Empty  
  Node l x r -> Node (mapTree f l) (f x) (mapTree f r)

{- | Equational Reasoning
>>> t0 = Node Empty 42 Empty
>>> mapTree show t0 
(case t0 of Empty -> Empty; Node l x r -> Node (mapTree show l) (show x) (mapTree show r))
(case Node Empty 42 Empty of Empty -> Empty; Node l x r -> Node (mapTree show l) (show x) (mapTree show r))
Node (mapTree show Empty) (show 42) (mapTree show Empty)
= Node Empty "42" Empty
-}

-- | Lookup a value in a tree, must be a binary search tree!
lookupTree :: Ord a => a -> Tree a -> Maybe a
lookupTree x tree = undefined

-- * Arithmetic quiz ----

data Expr
  = Num Int
  | Add Expr Expr
  | Mul Expr Expr
  deriving (Eq)

e1, e2 :: Expr
e1 = Mul (Add (Num 1) (Num 2)) (Num 3)
e2 = Add (Num 1) (Mul (Num 2) (Num 3))

-- ** Showing expressions

showExpr :: Expr -> String
showExpr expr = case expr of
  Num n -> show n
  Add x y -> showExpr x ++ " + " ++ showExpr y
  Mul x y-> showFactor x ++ " * " ++ showFactor y
  where
    showFactor (Add x y) = "(" ++ showExpr (Add x y) ++ ")"
    showFactor e         = showExpr e

instance Show Expr where 
  show = showExpr

instance Num Expr where 
  (+) = Add
  (*) = Mul
  fromInteger = Num . fromInteger

-- ** Evaluate (compute the value of) an expression ----

eval :: Expr -> Int
eval expr = case expr of
  Num n   -> n
  Add x y -> eval x + eval y
  Mul x y -> eval x * eval y

-- ** Generating arbitrary expressions


-- | Random generator (first version, with bad control over the size)
genExprBad :: Gen Expr
genExprBad = oneof [genNum, genOp]
  where
    genNum = do
      n <- choose (0, 10)
      return (Num n)

    genOp = do
      op <- elements [Add, Mul]
      e1 <- genExprBad
      e2 <- genExprBad
      return (op e1 e2)

-- | Random generator with control over the size
genExpr :: Int -> Gen Expr
genExpr n
  | n < 2     = genNum
  | otherwise = genOp
  where
    genNum = do
      n <- choose (0, 10)
      return (Num n)

    genOp = do
      op <- elements [Add, Mul]
      m  <- choose (1, n-1)
      e1 <- genExpr (n - m)
      e2 <- genExpr m
      return (op e1 e2)

-- ** The main program ----
-- | Sets the difficulty of the quiz
difficulty :: Int
difficulty = 4

-- | Asks the user to solve an expressiojn
quiz :: IO ()
quiz = do 
  expr <- generate (genExpr difficulty)
  putStrLn $ "Solve this expression: " ++ showExpr expr
  putStr "> "
  answer <- readLn
  let result = eval expr
  putStrLn $ (if result == answer
    then "Well Done!"
    else "Wrong!") 
    ++ "\nThe answer is " ++ show result
    
-- TODO: Needs to run multiple times!

-- * Differentiation expressions ----

(.+) :: Expr -> Expr -> Expr
Num 0 .+ x = x
x .+ Num 0 = x
Num x .+ Num y = Num (x+y)
x .+ y = Add x y
