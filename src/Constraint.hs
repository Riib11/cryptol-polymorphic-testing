{-# LANGUAGE LambdaCase #-}

module Constraint where 

import qualified Data.List as List
import qualified Data.Map as Map
import Q
import Var

data Constraint
  -- ComplexRelation Re Expr Q
  = ComplexConstraint Re Expr Q
  | SimpleConstraint Var Re Expr Q
  deriving (Eq)
data Re = Eq | Leq | Geq deriving (Eq)
type Expr = Map.Map Var Q
data SimpleEquality = SimpleEquality Var Expr Q deriving (Eq) -- x = e + c
data ComplexEquality = ComplexEquality Expr Q deriving (Eq) -- e = c

showConstraint :: Constraint -> String
showConstraint (ComplexConstraint r e q) = unwords [ showExpr e, showRe r, showQ q ]
showConstraint (SimpleConstraint x r e q) = 
  if Map.null e then
    unwords [ show x, showRe r, showQ q ]
  else if q /= 0 then
    unwords [ show x, showRe r, showExpr e, "+", showQ q ]
  else
    unwords [ show x, showRe r, showExpr e ]

showConstraints :: [Constraint] -> String
showConstraints cons = "[" ++ List.intercalate ", " (showConstraint <$> cons) ++ "]"

showRe :: Re -> String
showRe Eq = "="
showRe Geq = ">="
showRe Leq = "<="

showExpr :: Expr -> String
showExpr m | Map.null m = "0" 
showExpr m | otherwise = 
  List.intercalate " + " $ 
  fmap (\(x, q) -> showQ q ++ show x) $ 
  Map.toList m

showSimpleEquality :: SimpleEquality -> String 
showSimpleEquality (SimpleEquality x e q) = show x ++ " = " ++ showExpr e ++ " + " ++ showQ q

showComplexEquality :: ComplexEquality -> String
showComplexEquality (ComplexEquality e q) = showExpr e ++ " = " ++ showQ q

-- utilities

-- makes into simple constraint if there is only one variable
normConstraint :: Constraint -> Constraint
normConstraint (SimpleConstraint x r e q) = 
  SimpleConstraint x r (normExpr e) q
normConstraint (ComplexConstraint r e q) =
  let e' = normExpr e in
  -- can be made into simple constraint if has only one variable
  if Map.size e' == 1 then
    let [(x, a)] = Map.toList e' in
      SimpleConstraint x r mempty (q/a)
  else
    ComplexConstraint r e' q


mkConstraint :: ([(String, Q)], Re, Q) -> Constraint
mkConstraint (tms, r, q) = 
  let e = Map.fromList $ (\(s, q) -> (Var s, q)) <$> tms in 
    ComplexConstraint r e q

fromSimpleEqualityToConstraint :: SimpleEquality -> Constraint
fromSimpleEqualityToConstraint (SimpleEquality x e q) = SimpleConstraint x Eq e q

isSimpleConstraint :: Constraint -> Bool
isSimpleConstraint (SimpleConstraint _ _ _ _) = True
isSimpleConstraint _ = False

isSimpleEquality :: Constraint -> Bool 
isSimpleEquality (SimpleConstraint _ Eq _ _) = True 
isSimpleEquality _ = False

subConstraintByConstraints :: [Constraint] -> Constraint -> Constraint
subConstraintByConstraints cons con =  foldr f con cons
  where 
    f :: Constraint -> Constraint -> Constraint
    f conSimpleEquality@(SimpleConstraint x1 Eq e1 q1) = \case
      -- x1 = e2 + q2 = e1 + q1
      SimpleConstraint x2 Eq e2 q2 | x1 == x1 ->
        ComplexConstraint Eq (e1 `addExpr` negateExpr e2) (q1 - q2)
      -- x2 ~ e2 + q2
      SimpleConstraint x2 r e2 q2 | x1 /= x1 ->
        SimpleConstraint x2 r (substituteExpr x1 e1 e2) 
          (if Map.member x1 e2 then q2 + (e2 Map.! x1) * q1 else q2)
        
      -- e1 ~ q2
      ComplexConstraint r e2 q2 ->
        ComplexConstraint r (substituteExpr x1 e1 e2) 
          (if Map.member x1 e2 then q2 - (e2 Map.! x1) * q1 else q2)

-- substitute x for e' in e
substituteExpr :: Var -> Expr -> Expr -> Expr 
substituteExpr x e' e = 
  if Map.member x e
    then Map.delete x e `addExpr` (((e Map.! x) *) `mapExpr` e')
    else e

normComplexEquality (ComplexEquality e q) = ComplexEquality (normExpr e) q

normExpr :: Expr -> Expr 
normExpr = Map.filter (0 /=)

addExpr :: Expr -> Expr -> Expr 
addExpr = Map.unionWith (+)

subExpr :: Expr -> Expr -> Expr 
subExpr e1 e2 = e1 `addExpr` negateExpr e2

mapExpr :: (Q -> Q) -> Expr -> Expr
mapExpr = fmap

negateExpr :: Expr -> Expr 
negateExpr = fmap negate

isConstantExpr :: Expr -> Bool
isConstantExpr = (0 ==) . Map.size

isSingletonExpr :: Expr -> Bool
isSingletonExpr = (1 ==) . Map.size

freshVarExpr :: () -> Expr 
freshVarExpr _ = Map.singleton (freshVar ()) 1