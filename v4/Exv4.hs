module V4.Exv4 where

-- ! Excercises week 4: IO and testing --

-- ** Self-check
-- ? Basic IO

-- | Asks for the users first and last name and then prints it
printFullName :: IO ()
printFullName = do
    putStr "What is your first name?\n> "
    firstname <- getLine
    putStr "What is your last name?\n> "
    lastname <- getLine
    putStrLn ("Your full name is: " ++ firstname ++ " " ++ lastname)


-- ? Generating even integers

genEven :: Gen Integer
genEven = undefined

-- ** Basic


-- ** Challenging

