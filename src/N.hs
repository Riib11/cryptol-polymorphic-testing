module N where

import Data.String
import Prelude hiding (gcd, lcm)
import Data.List
import System.IO.Unsafe (unsafePerformIO)

-- | N, the natural numbers

data N = Z | S N deriving (Eq)

instance Ord N where 
  0 <= _ = True
  _ <= 0 = False
  S m <= S n = m <= n

instance Enum N where 
  toEnum i | i <= 0 = Z
  toEnum i = S (toEnum (i - 1))

  fromEnum Z = 0
  fromEnum (S n) = 1 + fromEnum n

instance Show N where show = show . fromEnum

instance Num N where
  0 + m = m
  S n + m = S (n + m)

  0 * m = 0
  S n * m = m + (n * m)

  signum 0 = 0
  signum (S _) = 1

  abs = undefined
  negate = undefined

  fromInteger i = toEnum (fromEnum i)

monus :: N -> N -> N
0 `monus` _ = 0
m `monus` 0 = m
S m `monus` S n = monus m n

instance Real N where
  toRational = toEnum . fromEnum

instance Integral N where 
  quotRem m n = go m 0 
    where 
      go m i | m >= n = go (m `monus` n) (i + 1)
      go m i | m <  n = (i, m)
  
  toInteger = toInteger . fromEnum

-- "m | n"
divides :: N -> N -> Bool
divides m n = l == 0 where (_, l) = n `quotRem` m

-- division which discards remainder
divus :: N -> N -> N
divus m n = l where (l, _) = m `quotRem` n

-- | list of prime numbers less than or equal to `n`, in decreasing order
primes :: N -> [N]
primes n = 
  if n < 2 
    then []
    else
      let ps = primes (n `monus` 1) in
      if any (`divides` n) ps
        then ps
        else n : ps

-- | factor an `N` into a product of primes, in decreasing order
factor :: N -> [N]
factor n = go n (primes n)
  where 
    go n ps | n == 1 || null ps = []
    go n (p : ps) | (m, 0) <- n `quotRem` p = p : go m (p : ps)
    go n (p : ps) = go n ps
    go n ps = error (show (n, ps))
    
areCoprime :: N -> N -> Bool 
areCoprime m n = null $ intersect (factor m) (factor n)

-- gcd -- greatest common divisor
gcd :: N -> N -> N
gcd m n = go m n (primes (max m n))
  where 
    go m n ps | m == 1 || n == 1 || null ps = 1
    go m n (p : ps) 
      | (m', 0) <- m `quotRem` p
      , (n', 0) <- n `quotRem` p
      = p * go m' n' (p : ps)
    go m n (p : ps) = go m n ps

gcdList :: [N] -> N
gcdList [] = 1
gcdList ns@(n:_) = foldr gcd n ns

-- | lcm -- greatest common multiple
lcm :: N -> N -> N
lcm m n = (m * n) `divus` gcd m n

lcmList :: [N] -> N
lcmList [] = 0
lcmList ns@(n:_) = foldr lcm n ns

instance IsString N where
  fromString = fromInteger . read