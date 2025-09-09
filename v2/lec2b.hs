import Prelude hiding (length)

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