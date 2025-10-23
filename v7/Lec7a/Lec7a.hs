
-- Modular Programming
    -- Modules (import, export)
-- Data structures
    -- Abstract data type (ADT)
    -- lookup tables (map)
    -- Implementation
    -- List
    -- Binary Treees


-- * Modular Programming
-- A good program is written in code that is easy to (further) develop
-- We want code to be:
    -- Re usable
    -- Extensible
    -- Maintainable (simple, readable, testable, robust)

-- Separation of concern, systematic abstraction
 -- Focus on one aspect at a time, keep al other aspects as abstract as possible, 
 -- such that you can understand and decide how to handle that aspect
 -- A general principle for programming

-- ** Modular design
 -- A programming system is usually too large to understand as a whole
 -- we need to divide it into subsystems to be able to understand it
 -- this process is called: decomposition
-- A modular system is divided into recognisable abstractions on different levels.Action

-- *** Advantages of proper modular design
-- Division of responsibility
-- Reduced complexity
-- Easy to extend
-- Modules can be:
    -- reused
    -- replaced (swapped out)
    -- tested in isolation
-- Enables simultaneous development

-- ** Modules in Haskell
-- Modules in Haskell enable:
    -- encapsulation (information hiding)
    -- reuse
    -- abstraction
    -- separation of namespaces (scope)
-- A Haskell module is stored in a single source file with the same name
-- A module exports and imports funcitonality
-- It is possible to export everything by omitting the export list
-- The export list is the module's interface (also called API)
    -- Good to hide constructors, only export type and operations

{- Example: 
module Table
    ( -- * Type
      Table
      -- * Construction tables
      , empty, insert
      -- * Queryig tables
      , lookup
    ) where

    import Data.List
    import Test.QuickCheck
 -}

-- * Finding keys fast
-- Using Binary Search Trees
-- Instead of list of key value pairs we use a tree of key value pairs
-- Invariant: the tree is ordered
    -- Smaller keys to the left
    -- Larger keys to the right
-- Thanks to the invariant we know if we should go left or right
-- to find the name we are looking for

module Lec7a where

import Prelude hiding (lookup)
-- import TableList
import TableTree

type Name   = String
type Number = Int



phoneBook :: Table Name Number
phoneBook = insert "max" 1234567
          $ insert "kajsa" 7654321
          $ insert "molly" 13243546
          $ insert "mira" 867567432
          $ empty

-- f :: Name -> Table Name Number -> Table Name Number
-- f n (Table kvs) = undefined

main :: IO ()
main = do
    putStrLn "Welcome to the phonebook!\n"
    putStr "Who are you looking for?\n> "
    name <- getLine
    putStrLn $ case lookup name phoneBook of
        Just number -> name ++ " has number " ++ show number
        _           -> name ++ " is not in the phonebook"