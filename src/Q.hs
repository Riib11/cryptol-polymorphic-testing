module Q where

import Normal
import N
import Z
import GHC.Real

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
roundDown (Q a b) = undefined

roundUp :: Q -> Z
roundUp (Q a b) = undefined


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
      go n | (a', 0) <- toEnum a `quotRem` n
           , (b', 0) <- b `quotRem` n
           = Just $ Q (signum a * fromEnum a') b'
  