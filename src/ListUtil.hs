module ListUtil where 

import Data.List as List

splitBy :: Eq a => [a] -> [a] -> [[a]]
splitBy pat xs = go [] xs
  where 
    go acc [] = if null acc then [] else [acc]
    go acc xs = case stripPrefix pat xs of 
      Just xs' -> (if null acc then [] else [acc]) <> go [] xs'
      Nothing -> go (acc <> [head xs]) (tail xs)

trim :: (a -> Bool) -> [a] -> [a]
trim p = takeWhile (not . p) . dropWhile p

startsWith :: (a -> Bool) -> [a] -> Bool 
startsWith p [] = False
startsWith p (y:_) = p y