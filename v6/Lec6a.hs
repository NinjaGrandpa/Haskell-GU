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
 3      8
2  4  7  9
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
foldTree f b a = 

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

