module Normal where 

import N

class Normal a where
  {-# MINIMAL simplify #-}
  simplify :: a -> Maybe a 

  normalize :: a -> a
  normalize a = case simplify a of 
    Just a' -> normalize a'
    Nothing -> a

  normalize' :: N -> a -> Either a a
  normalize' Z a = Left a
  normalize' (S n) a = case simplify a of 
    Just a' -> normalize' n a'
    Nothing -> Right a
