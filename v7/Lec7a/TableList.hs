module TableList
 ( Table
 , empty, insert
 , lookup
 ) where

import Prelude hiding (lookup)
import Data.Maybe

data Table key val = Table [(key, val)] deriving Show

empty :: Table k v
empty = Table []

lookup :: Eq k => k -> Table k v -> Maybe v
lookup key (Table kvs) = case filter ((== key) . fst) kvs of
    []       -> Nothing
    [(_, v)] -> Just v
    _        -> error "Duplicate keys"

keys :: Table k v -> [k]
keys (Table kvs) = map fst kvs

member :: Eq k => k -> Table k v -> Bool
member key t = key `elem` keys t 

insert :: Eq k => k -> v -> Table k v -> Table k v
insert key val t@(Table kvs) 
    | key `member` t = t
    | otherwise      = Table ((key, val) : kvs)