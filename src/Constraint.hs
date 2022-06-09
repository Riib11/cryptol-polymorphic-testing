module Constraint where 

import qualified Data.Map as Map
import Q
import Var

data Constraint = Relation Re Expr Q
data Re = Eq | Leq | Geq deriving (Eq)
type Expr = Map.Map Var Q
data Equation = Equation Var Expr Q deriving (Eq) -- x = e + c

isSimpleConstraint :: Constraint -> Bool
isSimpleConstraint (Relation r e q) =
  r `elem` [Leq, Geq] && isSingletonExpr e

subConstraint :: [Equation] -> Constraint -> Constraint
subConstraint = undefined -- TODO

normConstraint :: Constraint -> Constraint
normConstraint (Relation r e q) = Relation r (normExpr e) q

normExpr :: Expr -> Expr 
normExpr = Map.filter (0 /=)

addExpr :: Expr -> Expr -> Expr 
addExpr = Map.unionWith (+)

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