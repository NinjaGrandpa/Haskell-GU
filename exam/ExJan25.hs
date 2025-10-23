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

prop_chop :: Property
prop_chop = chop 2 [1,2,3,4,5,6] === [[1,2],[3,4],[5,6]]
          .&&. chop 3 [1,2,3,4] === [[1,2,3],[4]]
          .&&. chop 42 [1,2,3,4,5] === [[1,2,3,4,5]]

-- ** 3.
data FUPster = FUPster {
    players :: [Player],
    deck    :: [Card]
}

data Card = Card {
    name       :: String,
    speciality :: Speciality,
    birthyear  :: Int
}

data Speciality = Programming | Logic | Types | Security | FormalMethods

data Player = Player {
    playerName     :: String,
    hand     :: [Card],
    fupsters :: Int
}

fups :: [FUPster]
