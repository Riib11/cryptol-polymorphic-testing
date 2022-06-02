{-# Language BlockArguments #-}

module EqualityCondition where

import Symbol
import Q
import Z
import N
import Field
import qualified Constraint as C

-- | EqualityCondition

data EqualityCondition dom
  = Equal dom dom

instance Show dom => Show (EqualityCondition dom) where 
  show (Equal a b) = "(" ++ show a ++ " = " ++ show b ++ ")"

-- | convert a `Condition` into an `EqualityCondition`

toEqualityCondition :: (Field dom, Symbolic dom) => C.Condition dom -> EqualityCondition dom
toEqualityCondition (C.Re C.Eq a1 a2) = Equal a1 a2
toEqualityCondition (C.Re C.Le a1 a2) = Equal a1 (freshSymbol () .+ a1)
toEqualityCondition (C.Re C.Lt a1 a2) = Equal a1 (one .+ freshSymbol () .+ a1)
toEqualityCondition (C.Re C.Ge a1 a2) = toEqualityCondition (C.Re C.Le a2 a1)
toEqualityCondition (C.Re C.Gt a1 a2) = toEqualityCondition (C.Re C.Lt a2 a1)