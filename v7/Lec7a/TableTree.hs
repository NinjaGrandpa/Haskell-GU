module TableTree 
    (   -- * Type
        -- Table
        -- * Construction
        -- , empty, insert
        -- * Querying
        -- , lookup
        -- * Conversion
        -- , fromList, toList
    ) where

import Prelude hiding (lookup)
import qualified Data.Tree as T
import Test.QuickCheck

data Table k v = T