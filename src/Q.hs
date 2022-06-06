{-# LANGUAGE LambdaCase #-}

module Q where

import Data.String as String
import Data.List as List
import Normal
import N
import Z
import GHC.Real
import ListUtil

-- | Q

data Q = Q Z N

numerator :: Q -> Z 
numerator (Q a _) = a

denomenator :: Q -> N 
denomenator (Q _ b) = b

toZ :: Q -> Maybe Z
toZ q | Q a b <- normalize q, b == 1 = Just a
toZ _ = Nothing

fromZ :: Z -> Q
fromZ a = Q a 1

roundDown :: Q -> Z
roundDown q =
  if a >= 0 then 
    let (x, r) = toN a `quotRem` b in
      fromIntegral x
  else
    let (x, r) = toN (negate a) `quotRem` b in
      if r == 0 
        then -(fromIntegral x)
        else -(fromIntegral x) - 1
  where 
    Q a b = normalize q

roundUp :: Q -> Z
roundUp q =
  if a >= 0 then 
    let (x, r) = toN a `quotRem` b in
      if r == 0
        then fromIntegral x
        else fromIntegral x + 1
  else
    let (x, r) = toN (negate a) `quotRem` b in
      -(fromIntegral x)
  where 
    Q a b = normalize q

instance Show Q where 
  show (Q a b) = "(" ++ show a ++ "/" ++ show b ++ ")"

instance Eq Q where
  q1 == q2 = 
    let Q a1 b1 = normalize q1
        Q a2 b2 = normalize q2
    in a1 == a2 && b1 == b2

instance Ord Q where
  q1 <= q2 | Q a1 b1 <- normalize q1, Q a2 b2 <- normalize q2 = a1 * fromEnum b2 <= a2 * fromEnum b1

instance Num Q where 
  Q a1 b1 + Q a2 b2 | a2 /= 0 && a1 /= 0 = 
    normalize $ Q ((a1 * fromEnum b1) + (a2 * fromEnum b2)) (b1 * b2)
  Q a1 b1 + Q a2 b2 | a2 == 0 = normalize $ Q a1 b1
  Q a1 b1 + Q a2 b2 | a1 == 0 = normalize $ Q a2 b2
  
  Q a1 b1 * Q a2 b2 = normalize $ Q (a1 * a2) (b1 * b2)

  abs (Q a b) = Q (abs a) b
  signum (Q a b) = Q (signum a) 1
  fromInteger i = Q (fromInteger i) 1
  negate (Q a b) = Q (negate a) b

instance Real Q where
  toRational q | Q a b <- normalize q = toInteger a :% toInteger b

instance Fractional Q where
  fromRational (a :% b) = normalize $ Q (fromInteger a) (fromInteger b)
  recip (Q a b) = Q (signum a * fromIntegral b) (toN a) 

instance Normal Q where
  simplify (Q a b) | a == 0 && b == 1 = Nothing
  simplify (Q a b) | a == 0 = Just 0
  simplify (Q a b) = go (N.gcd (toEnum a) b)
    where 
      go n | n == 1 = Nothing
      go n | (a', 0) <- toEnum (abs a) `quotRem` n
           , (b', 0) <- b `quotRem` n
           = Just $ Q (signum a * fromEnum a') b'
      go n = Nothing
  -- simplify (Q a b) | a < 0 = go (N.gcd (toEnum na) b) 
  --   where 
  --     na = negate a
  --     go n | n == 1 = Nothing
  --     go n | (a', 0) <- toEnum na `quotRem` n
  --          , (b', 0) <- b `quotRem` n
  --          = Just $ Q (signum a * fromEnum a') b'
  

instance IsString Q where 
  fromString =
    (\case 
      [a] -> Q (read a) 1
      -- [a, b] -> error $ show [a,b] -- Q (read a) (fromString b)
      [a, b] -> Q (read a) (fromString b)
    ) .
    splitBy "/" .
    trim (`elem` [' ', '(', ')'])