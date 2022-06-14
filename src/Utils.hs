module Utils where

import qualified Data.List as List
import qualified Data.Map as Map
import GHC.Real

mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
mapWithIndex f as = go 0 as
  where 
    go i [] = []
    go i (a : as) = f i a : go (i + 1) as

modifyAtList :: Int -> (a -> a) -> [a] -> [a]
modifyAtList _ f [] = []
modifyAtList 0 f (x:xs) = f x : xs
modifyAtList i f (x:xs) = x : modifyAtList (i-1) f xs

deleteAtList :: Int -> [a] -> [a]
deleteAtList _ [] = []
deleteAtList 0 (x:xs) = xs
deleteAtList i (x:xs) = x : deleteAtList (i-1) xs

concatMapWithKey :: (k -> v -> [a]) -> Map.Map k v -> [a]
concatMapWithKey f = Map.foldrWithKey (\k a as -> f k a <> as) []

(==>) :: Bool -> Bool -> Bool
p ==> q = if p then q else True