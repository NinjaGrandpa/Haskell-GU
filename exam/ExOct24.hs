-- Exam October 2024 --

module ExOct24 where

import Test.QuickCheck hiding (Some)
import Data.Maybe

-- * Part 1

-- ** 1.
-- *** a)
{- 
fun [1,2,3]
= { applying fun }
1 + fun [2,3]
=
1 + (1 + fun [3])
=
1 + (1 + 3)
=
5
 -}

-- *** b)
fun :: Num a => [a] -> a
fun [] = 10
fun [x] = x
fun (x:xs) = 1 + fun xs

-- ** 2.

-- | Creates a new list where the elements of the two input lists are interleaved
interleave :: [a] -> [a] -> [a]
interleave xs [] = xs
interleave [] ys = ys
interleave (x:xs) (y:ys) = x:y:interleave xs ys

-- ** 3.
data GhChat = GhChat {
    serverAddress :: String,
    users :: [User],
    channels :: [Channel]
}

data User = User String Role 

data Channel = Channel String [Post]

data Post = Post String User

data Role = Admin | Member | Bot

-- ** 4.

-- | Reads the name and role of a user
readUser :: IO (String, Role)
readUser = do
    putStr "Please give your user name:\n> "
    name <- getLine
    role <- getRole name
    return (name, role)

getRole :: String -> IO Role
getRole s = undefined

-- ** 5.

data Some a = One a | Two a a deriving (Eq, Show)

-- | Converts a list of Somes to an ordinary list:
flatten :: [Some a] -> [a]
flatten [] = []
flatten ((One x):xs) = x : flatten xs
flatten ((Two x y):xs) = x : y : flatten xs

-- flat :: Some a -> [a]
-- flat (One x) = [x]
-- flat (Two x y) = [x, y]

-- ** 6.

-- | Validates the sum of two input integer lists is equal to the sum of two interleaved lists
prop_sum :: [Int] -> [Int] -> Bool 
prop_sum xs ys = sum xs + sum ys == (sum $ interleave xs ys)

-- ** 7.
type Name = String
type Number = Int
data PhoneBook = Empty | Insert Name Number PhoneBook deriving Show

phoneBook :: PhoneBook
phoneBook = Insert "Max" 43495 
          (Insert "Alex" 6154 
          (Insert "Kajsa" 5678 Empty))



filterBook :: (Name -> Bool) -> PhoneBook -> PhoneBook
filterBook pred pb = case pb of
    Empty -> Empty
    Insert n nmb npb
        | pred n    -> Insert n nmb $ filterBook pred npb
        | otherwise -> filterBook pred npb

-- * Part 2.
-- ** 8.

data Expr
    = Val Value
    | Add Expr Expr
    | Mul Expr Expr
    | And Expr Expr
    | Or Expr Expr
    | If Expr Expr Expr
    deriving (Show, Eq)

data Value = Num Int | Bool Bool deriving (Show, Eq)

int :: Int -> Expr
int n = Val (Num n)

true, false :: Expr
true = Val (Bool True)
false = Val (Bool False)

num :: (Int -> Int -> Int) -> Expr -> Expr -> Value
num op l r = case (eval l, eval r) of
    (Num x, Num y) -> Num (x `op` y)
    _              -> error "Type error: expected two integer values"

bool :: (Bool -> Bool -> Bool) -> Expr -> Expr -> Value
bool op l r = case (eval l, eval r) of
    (Bool a, Bool b) -> Bool (a `op` b)
    _                -> error "Type error: expected two integer values" 

eval :: Expr -> Value
eval expr = case expr of
    Val v    -> v
    Add x y  -> num (+) x y
    Mul x y  -> num (*) x y
    And a b  -> bool (&&) a b 
    Or a b   -> bool (||) a b   
    If c t e -> case eval c of
        Bool b -> eval (if b then t else e)
        _      -> error "Type error: the condition must be a boolean"

prop_eval :: Bool
prop_eval = 
    eval (If true (Add (int 1) (int 2)) (int 42)) 
        == Num 3
    && eval (If (And true false) (Add (int 1) (int 2)) (int 42)) 
        == Num 42
    && eval (Or true false)
        == Bool True

-- ** 9.

data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving Show
type List a = [(Tree a, Int)]

-- | Creates an empty list 
empty :: List a
empty = []

-- | Adds an element to a list
add :: a -> List a -> List a
x `add` ((l, n) : (r, m) : ts) | n == m = (Node l x r, 2 * n + 1) : ts
x `add` ts                              = (Leaf x, 1) : ts

-- *** a)
-- | Counts the number of elements in a List
size :: List a -> Int
size [] = 0
size ((_, n):ts) = n + size ts

-- *** b)
-- | Converts a list into a type List
fromList :: [a] -> List a
fromList = foldr add empty

-- *** c)
-- | Returns head of a List
hd :: List a -> a
hd ((t, n):ts) = case t of
    Leaf x -> x
    Node l x r -> x 
