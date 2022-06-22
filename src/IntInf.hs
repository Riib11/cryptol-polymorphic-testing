{-# LANGUAGE PatternSynonyms #-}

module IntInf where 

data IntInf = Fin Int | Inf Int

pattern PosInf = Inf 1
pattern NegInf = Inf (-1)

norm (Inf 0) = Fin 0
norm (Inf i) = Inf (signum i)
norm x = x

rawEq :: IntInf -> IntInf -> Bool 
rawEq (Fin i) (Fin j) = i == j
rawEq (Inf i) (Inf j) = i == j
rawEq _ _ = False

instance Eq IntInf where
  x == y = norm x `rawEq` norm y

instance Show IntInf where 
  show (Fin i) = show i 
  show (Inf i) | i <  0 = "-inf"
  show (Inf i) | i == 0 = "0inf"
  show (Inf i) | i >  0 = "+inf"

instance Num IntInf where
  Fin i + Fin j = Fin $ i + j
  Fin i + Inf j = norm $ Inf j
  Inf i + Fin j = norm $ Inf j
  Inf i + Inf j | signum i == signum j = norm $ Inf $ signum i
  Inf i + Inf j | signum i /= signum j = error $ "undefined: " ++ show (Inf i) ++ " + " ++ show (Inf j)
  
  Fin i * Fin j = Fin $ i * j
  Fin i * Inf j = norm $ Inf j
  Inf i * Fin j = norm $ Inf j
  Inf i * Inf j = norm $ Inf (i * j)

  negate (Fin i) = Fin (negate i)
  negate (Inf i) = norm $ Inf (negate i)

  abs (Fin i) = Fin (abs i)
  abs (Inf i) = norm $ Inf (abs i)

  signum (Fin i) = Fin (signum i)
  signum (Inf i) = norm $ Inf (signum i)

  fromInteger = Fin . fromInteger  

instance Ord IntInf where
  Fin i <= Fin j = i <= j
  
  Fin i <= Inf (-1) = False
  Fin i <= Inf 0 = i <= 0
  Fin i <= Inf 1 = True

  Inf (-1) <= Fin j = True
  Inf 0 <= Fin j = j >= 0
  Inf 1 <= Fin j = False

