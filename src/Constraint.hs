module Constraint where 

import Data.Set as Set
import Symbol
import Expr

data Constraint
  = Relation Re Expr Expr 
  | Typeclass Symbol Tc
  deriving (Eq)

data Re = Eq | Leq | Geq deriving (Eq, Show)

data Tc = Fin | Prime deriving (Eq, Show)

instance Show Constraint where 
  show (Relation r e1 e2) = case r of 
    Eq  -> s1 ++ " == " ++ show e2
    Leq -> s1 ++ " <= " ++ show e2
    Geq -> s1 ++ " >= " ++ show e2
    where
      s1 = case e1 of 
        _ | [x] <- Set.toList $ symbolSet e1 -> show x
        _ | otherwise -> show e1
  show (Typeclass x tc) = case tc of 
    Fin -> "fin(" ++ show x ++ ")"
    Prime -> "prime(" ++ show x ++ ")"

symbolSetConsraint :: Constraint -> Set.Set Symbol
symbolSetConsraint (Relation re e1 e2) = symbolSet e1 <> symbolSet e2
symbolSetConsraint (Typeclass x _) = Set.singleton x

symbolExprEqualityLhs :: Constraint -> [(Symbol, Expr)]
symbolExprEqualityLhs (Relation re e1 e2) 
  | [x] <- Set.toList $ symbolSet e1 = [(x, e2)]
  | otherwise = []
symbolEqualityLHS _ = []

symbolSetEqualityRhs :: Constraint -> Set.Set Symbol
symbolSetEqualityRhs (Relation re e1 e2) = symbolSet e2
symbolSetEqualityRhs _ = mempty

negateRe :: Re -> Re
negateRe Eq = Eq 
negateRe Geq = Leq
negateRe Leq = Geq
