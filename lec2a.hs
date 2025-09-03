-- :t command show the type of any expression
-- The "::"-symbol indicates a type signature, "is of type"

-- Prelude> :type True
-- True :: Bool
-- Prelude> :type False
-- False :: Bool
-- Prelude> :t (3 < 5)
-- (3 < 5) :: Bool

-- chr :: Int -> Char, decodes int to char
-- ord :: Char -> Int, encodes a char to a numeric code
-- use the :module or :m Data.Char to import the module where the commands are defined

-- Functions with more than one argument

-- Write the type signature above the corresponding function definition
xor :: Bool -> Bool -> Bool
xor p q = (p || q) && not (p && q)

-- Type inference
isL :: Char -> Bool
isL c = c == 'l'

-- Both arguments of "==" must have the same type
-- Can write type signatures of the same type in one signature separated by commas
firstName, lastName :: String
firstName = "Max"
lastName = "Jenslov"
