{-# LANGUAGE MultiWayIf #-}

module Solving where 

import GHC.Real
import qualified Data.Set as Set
import qualified Data.Map as Map
import Symbol
import Expr
import Constraint
import M

-- makes all constants and coefficients into integers
eliminateDenomenators :: Constraint -> M Constraint
eliminateDenomenators (Relation r e1 e2) = 
  let 
    rats = [constant e1] <> coefficients e1 <> 
           [constant e2] <> coefficients e2
    dnms = (\(_ :% dnm) -> dnm) <$> rats
    elimDnm r = r * fromIntegral (foldr lcm 1 dnms)
  in 
    pure $ Relation r (elimDnm `mapExpr` e1) (elimDnm `mapExpr` e2)
eliminateDenomenators (Typeclass x tc) = pure $ Typeclass x tc

-- a1[x1] ~ a2[x2] --> a1[x1] - a2[x2] ~ 0
-- c*x + a1[x1] - a2[x2] ~ 0 --> 
-- x + a1[x1] ~ 0 --> x ~ -a1[x1]
-- yields form: x ~ a[y]
solveForOneVariable :: Constraint -> M Constraint
solveForOneVariable (Relation r e1 e2) = do
  -- move everything to LHS
  let lhs = e1 `addExpr` (negate `mapExpr` e2)
  -- choose variable
  x <- choose $ Set.toList (symbolSet lhs)
  -- divide by coefficient of x
  let a = coefficient x lhs
  let lhs' = (/ a) `mapExpr` lhs
  -- move everything except x to RHS
  let rhs = negate `mapExpr` deleteSymbol x lhs'
  --
  pure $ Relation (if a >= 0 then r else negateRe r) (fromSymbol x) rhs
solveForOneVariable (Typeclass x tc) = pure $ Typeclass x tc

-- x >= a[y] --> x = ceil(a[y]) + fresh()
-- requires form: x ~ a[y]
expandGeq :: Constraint -> M Constraint
expandGeq (Relation Geq a1 a2) = do
  let r' = Relation Eq a1 (a2 `addExpr` fresh ())
  debug $ show (Relation Geq a1 a2) ++ " --> " ++ show r'
  pure r'
expandGeq con = pure con

-- requires form: x ~ a[y]
-- • a[x] - b[y] + c --> b[y] <= a[x] + c
-- • a[x] - b[y] - c --> b[y] + c <= a[x]
enforceSafeSubtraction :: Constraint -> M [Constraint]
enforceSafeSubtraction (Relation r e1 e2) | isSingleton e1 = do 
  let 
    c = constant e2 
    ts = terms e2
    tsNeg = Map.filter (<  0) ts
    tsPos = Map.filter (>= 0) ts
  if
    | Map.null tsNeg -> pure [ Relation r e1 e2 ]
    | c >= 0 -> pure 
        [ Relation r e1 e2
        , Relation Leq (Expr 0 (negate <$> tsNeg)) (Expr c tsPos) ]
    | c < 0 -> pure 
        [ Relation r e1 e2
        , Relation Leq (Expr (negate c) (negate <$> tsNeg)) (Expr 0 tsPos) ]
      
enforceSafeSubtraction (Typeclass x tc) = pure $ [Typeclass x tc]

solve :: [Constraint] -> M [Constraint]
solve cons = do
  cons <- traverse eliminateDenomenators cons
  cons <- traverse solveForOneVariable cons
  cons <- traverse expandGeq cons
  cons <- mconcat <$> traverse enforceSafeSubtraction cons
  pure cons
