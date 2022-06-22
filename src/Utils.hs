module Utils where

import qualified Data.List as List
import qualified Data.Map as Map
import GHC.Real

mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
mapWithIndex f = map (uncurry f) . zip [0..]

modifyAtList :: Int -> (a -> a) -> [a] -> [a]
modifyAtList _ f [] = []
modifyAtList 0 f (x:xs) = f x : xs
modifyAtList i f (x:xs) = x : modifyAtList (i-1) f xs

setAtList :: Int -> a -> [a] -> [a]
setAtList i a = modifyAtList i (const a)

deleteAtList :: Int -> [a] -> [a]
deleteAtList _ [] = []
deleteAtList 0 (x:xs) = xs
deleteAtList i (x:xs) = x : deleteAtList (i-1) xs

swapAtList :: Int -> Int -> [a] -> [a]
swapAtList i i' xs = setAtList i (xs!!i') . setAtList i' (xs!!i) $ xs

pullToHead :: Int -> [a] -> [a]
pullToHead i xs = xs!!i : deleteAtList i xs

concatMapWithKey :: (k -> v -> [a]) -> Map.Map k v -> [a]
concatMapWithKey f = Map.foldrWithKey (\k a as -> f k a <> as) []

(==>) :: Bool -> Bool -> Bool
p ==> q = if p then q else True