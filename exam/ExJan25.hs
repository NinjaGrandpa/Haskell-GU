module ExJan25 where

import Test.QuickCheck

-- * Part 1.
-- ** 1.
fun [] = 0
fun (x:xs) = if even x then x * fun xs else x + fun xs

-- *** a)
{- 
fun [2, 21]
= { applying fun, applying even }
2 * fun [21]
= { applying fun }
2 * 21 + fun []
= { applying fun }
2 * 21 + 0
= { applying *, +}
42
 -}

-- *** b)
fun :: Integral a => [a] -> a

-- ** 2.

-- | Chops a list into sub-lists of n size
chop :: Int -> [a] -> [[a]]
chop 0 _ = [[]]
chop _ [] = [[]]
chop n xs = case (splitAt n xs) of
    (ys, []) -> [ys]
    (ys, zs) -> ys : chop n zs 

prop_chopa :: Property
prop_chopa = chop 2 [1,2,3,4,5,6] === [[1,2],[3,4],[5,6]]
          .&&. chop 3 [1,2,3,4] === [[1,2,3],[4]]
          .&&. chop 42 [1,2,3,4,5] === [[1,2,3,4,5]]

-- ** 3.
data FUPster = FUPster {
    players :: [Player],
    deck    :: [Card]
}

data Card = Card {
    name       :: String,
    birthyear  :: Int,
    speciality :: Speciality
}

data Speciality = Programming | Logic | Types | Security | FormalMethods

data Player = Player {
    playerName     :: String,
    hand     :: [Card],
    fupsters :: Int
}

cards :: [Card]
cards =
  [ Card { name       = "Alonzo Church"
         , speciality = Logic
         , birthyear  = 1903
         }
  , Card { name       = "Alan Turing"
         , speciality = FormalMethods
         , birthyear  = 1912
         }
  , Card { name       = "John Backus"
         , speciality = Programming
         , birthyear  = 1924
         }
  , Card { name       = "Robin Milner"
         , speciality = Types
         , birthyear  = 1934
         }
  , Card { name       = "Phil Wadler"
         , speciality = Types
         , birthyear  = 1956
         }
  , Card { name       = "John Hughes"
         , speciality = Programming
         , birthyear  = 1958
         }
  , Card { name       = "Paul Hudak"
         , speciality = Programming
         , birthyear  = 1952
         }
  , Card { name       = "Simon Peyton Jones"
         , speciality = Types
         , birthyear  = 1958
         }
  ]

-- ** 4.

readCard :: IO Card
readCard = do
    putStr "Please give the legend name:\n> "
    name <- getLine
    putStrLn "Year of birth:\n> "
    y <- readLn
    putStrLn "Speciality:\n> "
    s <- getLine
    spec <- getSpeciality s
    return (Card name y spec)

getSpeciality :: String -> IO Speciality
getSpeciality s = undefined

-- ** 5.
data List = Empty | Skip List | Cons Int List deriving Show

xs, ys :: List
xs = Cons 1 (Cons 2 (Skip (Cons 4 Empty)))
ys = Cons 1 (Skip (Cons 3 (Skip (Cons 5 Empty))))

-- | Converts a list of type List into a list of Maybe:s
toList :: List -> [Maybe Int]
toList xs = case xs of
    Empty -> []
    Skip l -> Nothing : toList l
    Cons n l -> Just n : toList l
    

prop_toList :: Property
prop_toList = toList xs === [Just 1, Just 2, Nothing, Just 4] .&&.
              toList ys === [Just 1, Nothing, Just 3, Nothing, Just 5]

-- ** 6.

-- | Checks that chop returns sub-lists of size n
prop_chop :: Int -> [Int] -> Property
prop_chop n xs = 
    collect (length xs)$
