module Normal where 

class Normal a where
  {-# MINIMAL simplify #-}
  simplify :: a -> Maybe a 

  normalize :: a -> a
  normalize a = case simplify a of 
    Just a' -> normalize a'
    Nothing -> a

  normalize' :: Int -> a -> Either a a
  normalize' n a | n <= 0 = Left a
  normalize' n a = case simplify a of 
    Just a' -> normalize' n a'
    Nothing -> Right a
