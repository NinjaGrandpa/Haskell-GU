module TableTree 
    (   -- * Type
        Table
        -- * Construction
        , empty, insert
        -- * Querying
        , lookup
        -- * Conversion
        -- , fromList, toList
    ) where

import Prelude hiding (lookup)
import qualified Data.Tree as T
import Test.QuickCheck

data Table k v 
    = Empty
    | Node (Table k v) k v (Table k v)
    deriving Show

t :: Table String Int
t = Node (Node (leaf "Anna" 2048) "Dave" 1018 (leaf "John" 1001)) "Koen" 4567 (Node (leaf "Max" 12345) "Kajsa" 45678 (leaf "Molly" 45678))

leaf :: k -> v -> Table k v
leaf key val = Node Empty key val Empty

empty :: Table k v
empty = Empty

lookup :: Ord k => k -> Table k v -> Maybe v
lookup key table = case table of
    Empty -> Nothing
    Node l k v r
        | key == k -> Just v
        | key < k -> lookup key l
        | key > k -> lookup key r

insert :: Ord k => k -> v -> Table k v -> Table k v
insert key val table = case table of
    Empty          -> leaf key val
    Node l k v r 
        | key == k -> Node l key val r
        | key < k  -> Node (insert key val l) k v r
        | key > k  -> Node l k v (insert key val r)

invariant :: Ord k => Table k v -> Bool
invariant Empty = True
invariant (Node l k _ r) = all (< k) (keys l) && all (> k) (keys r)
                         && invariant l && invariant r
 
keys :: Table k v -> [k]
keys = map fst . toList

toList :: Table k v -> [(k, v)]
toList Empty = []
toList (Node l k v r) = toList l ++ [(k, v)] ++ toList r

fromList :: Ord k => [(k, v)] -> Table k v
fromList kvs = foldr (uncurry insert) Empty kvs
-- fromList kvs = foldr (\(k,v) t -> insert k v t) Empty kvs

genKeyVal :: Gen (Char, Int)
genKeyVal = do
    key <- elements ['a'..'z']
    val <- choose (0, 100)
    return (key, val)

genTable :: Gen (Table Char Int)
genTable = do
    kvs <- listOf genKeyVal
    return (fromList kvs)

prop_invariant :: Property
prop_invariant = forAll genTable invariant