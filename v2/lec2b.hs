import Prelude hiding (Just, Maybe, Nothing, length, null)

-- Polymorphic functions

lenInt :: [Int] -> Int
lenInt [] = 0
lenInt (_ : xs) = 1 + lenInt xs

-- Type error om används med argument som inte är Int

lenStr :: [String] -> Int
lenStr [] = 0
lenStr (_ : xs) = 1 + lenStr xs

-- Polymorphic Length
length :: [a] -> Int
length [] = 0
length (_ : xs) = 1 + length xs

-- Only works for lists of ints -> generalize
-- null :: [Int] -> Bool
-- null [] = True
-- null _ = False

null :: [a] -> Bool
null [] = True
null _ = False

-- Strings are Lists!

-- * type String = [Char]

str :: String
str = 'A' : "nna likes FP"

-- "Max" == 'M': ('a' : ('x' : []))

--------------------------
-- Type Variables

-- Define data type
data Maybe a = Just a | Nothing deriving (Show, Eq)

-- No crash on division by zero
safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just (x `div` y)

-- Just, Nothing, Maybe

maybeAdd :: Maybe Int -> Maybe Int -> Maybe Int
maybeAdd (Just x) (Just y) = Just (x + y)
maybeAdd _ _ = Nothing

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead x = Just (head x)

a :: (Num a) => a
a = 1

b :: Int
b = 1

-- A data type for a person

-- data Person = Person
--   { firstName :: String,
--     lastName :: String,
--     age :: Int
--   }

data Person = Person String String Int

maxj, molly, mira :: Person
maxj = Person "Max" "Jenslöv" 23
molly = Person "Molly" "Jenslov" 13
mira = Person "Mira" "Jenslöv" 11

family :: [Person]
family = [maxj, molly, mira]

-- Show instance of Person
instance Show Person where
  show (Person firstName lastName age) = firstName ++ " " ++ lastName ++ ", Age: " ++ "(" ++ show age ++ ")"

instance Eq Person where
  Person _ _ s1 == Person _ _ s2 = s1 == s2