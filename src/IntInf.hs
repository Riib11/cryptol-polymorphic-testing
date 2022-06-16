{-# LANGUAGE PatternSynonyms #-}
module IntInf where 

-- data IntInf 
--   = Int Int
--   | Inf Bool -- True: positive, False: negative
--   deriving (Eq)

-- pattern PosInf = Inf True
-- pattern NegInf = Inf False

-- instance Show IntInf where
--   show (Int x) = show x 
--   show PosInf = "inf"
--   show NegInf = "-inf"

-- instance Num IntInf where
--   Int x + Int y = Int (x + y)
--   --
--   NegInf + PosInf = error "-inf + inf"
--   PosInf + NegInf = error "inf + -inf"
--   --
--   NegInf + _ = NegInf
--   _ + NegInf = NegInf
--   -- 
--   PosInf + _ = PosInf
--   _ + PosInf = PosInf

--   negate (Int x) = Int (negate x)
--   negate (Inf b) = Inf (not b)

--   Int x * Int y = Int (x * y)
--   Inf 

  
--   NegInf * NegInf = PosInf

--   abs (Int x) = Int (abs x)
--   abs (Inf b) = Inf True

--   signum (Int x) = Int (signum x)
--   signum (Inf b) = Int (if b then 1 else -1)

--   fromInteger = Int . fromInteger

-- instance Ord IntInf where 
--   Int x <= Int y = x <= y
  
--   PosInf <= PosInf = True
--   PosInf <= _ = False 
--   _ <= PosInf = True

--   NegInf <= _ = True 
--   NegInf <= NegInf = True
--   _ <= NegInf = False
