module Utils where

mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
mapWithIndex f as = go 0 as
  where 
    go i [] = []
    go i (a : as) = f i a : go (i + 1) as
