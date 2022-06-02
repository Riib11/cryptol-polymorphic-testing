module Field where

-- | Field

class Field a where 
  zero :: a
  one :: a
  (.+) :: a -> a -> a
  (.-) :: a -> a -> a
  (.*) :: a -> a -> a
  (./) :: a -> a -> a
  (.^) :: a -> a -> a
  (.%) :: a -> a -> a

reciprocal :: Field a => a -> a
reciprocal a = one ./ a

negative :: Field a => a -> a 
negative a = zero .- a
