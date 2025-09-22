module Stud1 where
-- Functions

doubleMe x = x + x

doubleUs x y = x + x + y + y

doubleSmallNumber x = if x > 100 then x else x * 2

maxJenslöv = "Max Jenslöv"

-- Lists

lostNumbers = [4, 8, 15, 16, 23, 42]

-- Putting lists together with the ++ operator

listA = [1, 2, 3, 4]

listB = [5, 6, 7, 8]

listC = listA ++ listB -- [1,2,3,4,5,6,7,8]

helloWorld = "hello" ++ " " ++ "world"

-- ++ operator walks through the whole list on the left side of the "++"-operator
-- Watch out for using this operator with very large lists
-- ":"-operator puts something in the beginning of the list

smallCat = " SMALL CAT"

aSmallCat = 'A' : smallCat

numbers = 1 : [2, 3, 4, 5] -- [1,2,3,4,5]

-- The ":"-operator takes a single variable and a list instead of the "++"-operator which takes two lists

-- Use the "!!"-operator to get an element out of the list by index. The indices start at 0
character = "Max Jenslöv" !! 4 -- 'J'

fourthNumber = [1, 2, 3, 4, 5, 6] !! 4 -- 5

-- Lists can contain lists that contains lists

lists = [[1, 2, 3, 4], [5, 6, 7, 8], [9, 10, 11, 12]]




