module Q where

import Normal
import N
import Z
import Field

-- | Q

data Q = Q Z N

numerator :: Q -> Z 
numerator (Q a _) = a

denomenator :: Q -> N 
denomenator (Q _ b) = b

instance Show Q where 
  show (Q a b) = "(" ++ show a ++ "/" ++ show b ++ ")"

instance Eq Q where
  q1 == q2 = 
    let Q a1 b1 = normalize q1
        Q a2 b2 = normalize q2
    in a1 == a2 && b1 == b2

instance Ord Q where
  Q a1 b1 <= Q a2 b2 = a1 * fromEnum b2 <= a2 * fromEnum b1

instance Num Q where 
  (+) = (.+)
  (*) = (.*)
  abs (Q a b) = Q (abs a) b
  signum (Q a b) = Q (signum a) 1
  fromInteger i = Q (fromInteger i) 1
  negate (Q a b) = Q (negate a) b

instance Field Q where 
  zero = Q 0 1
  one = Q 1 1
  Q a1 b1 .+ Q a2 b2 = normalize $ Q ((a1 * fromEnum b1) + (a2 * fromEnum b2)) (b1 * b2)
  Q a1 b1 .- Q a2 b2 = normalize $ Q ((a1 * fromEnum b1) - (a2 * fromEnum b2)) (b1 * b2)
  Q a1 b1 .* Q a2 b2 = normalize $ Q (a1 * a2) (b1 * b2)
  Q a1 b1 ./ Q a2 b2 | a2 /= 0 = Q a1 b1 .* Q (signum a2 * fromEnum b2) (toEnum (abs a2))
  Q a1 b1 ./ Q a2 b2 | a2 == 0 = zero
  Q a1 b1 .^ Q a2 b2 | Q a2' b2' <- normalize (Q a2 b2), b2' == 1 = Q (a1 ^ a2') b1
  Q a1 b1 .^ Q a2 b2 | otherwise = error "Cannot take a Q to the power Q with denomenator /= 1"
  Q a1 b1 .% Q a2 b2 | Q a2' b2' <- normalize (Q a2 b2), b2' == 1 = Q (a1 `mod` a2') b1
  Q a1 b1 .% Q a2 b2 | otherwise = error "Cannot modulus a Q by a Q with denomenator /= 1"

instance Normal Q where
  simplify (Q a b) | a == 0 && b == 1 = Nothing
  simplify (Q a b) | a == 0 = Just zero
  simplify (Q a b) = go (N.gcd (toEnum a) b)
    where 
      go n | n == 1 = Nothing
      go n | (a', 0) <- toEnum a `quotRem` n
           , (b', 0) <- b `quotRem` n
           = Just $ Q (signum a * fromEnum a') b'
  