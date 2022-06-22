module RawConstraint where

import Q

data RawConstraint
  = Relation Rel Exp Exp
  | Typeclass Tc Var
data Rel = Equ | Leq | Geq
data Exp = Const Q | Var String | Op Op Exp Exp
data Op  = Add | Sub | Mul | Div | Exp | Mod
data Var = MkVar String
data Tc  = Prime | Fin

-- TODO: RawConstraint -> Maybe Constraints
-- rejects division by variable
-- rejects multiplication of variables
-- rejects exponentiation
-- substitutes away 