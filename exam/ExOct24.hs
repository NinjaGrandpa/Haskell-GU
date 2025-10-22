-- Exam October 2024 --

module ExOct24 where

import Test.QuickCheck hiding (Some)
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
phoneBook = Insert "Max" 43495 (Insert "Alex" 6154 Empty)

filterBook :: (Name -> Bool) -> PhoneBook -> PhoneBook
filterBook pred phbk = undefined
filterBook pred phbk = undefined
filterBook pred phbk = undefined