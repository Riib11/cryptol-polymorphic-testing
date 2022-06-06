module Constraint where 

import Control.Arrow
import Control.Monad
import qualified Data.Set as Set
import Control.Applicative
import Data.Maybe
import Symbol
import Normal
import N
import Z
import Q
import LinearArithmetic.Polynomial as P

-- | Constraint
data Constraint a
  = Relation Re (Polynomial a) (Polynomial a) -- a + sum(bi * xi) ~ c + sum(dj * yj)
  | Typeclass Symbol Typeclass

instance (Show a, Num a, Eq a) => Show (Constraint a) where
  show (Relation re p1 p2) = concat ["(", show p1, show re, show p2, ")"]
  show (Typeclass x tc) = unwords [show tc, show x]

-- primive relations
data Re = Eq | Le | Ge deriving (Eq)

instance Show Re where 
  show Eq = " = "
  show Le = " ≤ "
  show Ge = " ≥ "

flipRe :: Re -> Re 
flipRe Eq = Eq 
flipRe Le = Ge
flipRe Ge = Le

-- primitive typeclasses
data Typeclass = Prime | Fin 

instance Show Typeclass where 
  show Prime = "prime"
  show Fin = "fin"

-- | SolvedConstraint
data SolvedConstraint a 
  = SolvedRelation Symbol Re (Polynomial a) -- a ~ b + sum(ci * xi)
  | SolvedTypeclass Symbol Typeclass

instance (Show a, Num a, Eq a) => Show (SolvedConstraint a) where
  show (SolvedRelation x re p) = 
    "solved" ++ unwords ["(", show x, show re, show p, ")"]
  show (SolvedTypeclass x tc) =
    "solved" ++ unwords ["(", show tc, show x, ")"]

-- | SafeConstraint

data SafeConstraint
  = SafeRelation Symbol Re (Polynomial Z)
  | SafeTypeclass Symbol Typeclass

instance Show SafeConstraint where
  show (SafeRelation x re p) =
    "safe" ++ concat ["(", show x, show re, show p, ")"]
  show (SafeTypeclass x tc) = 
    "safe" ++ unwords ["(", show tc, show x, ")"]

-- | SampleRange

data SampleRange = SampleRange
  { upperBounds :: [Polynomial Z],
    typeclasses :: [Typeclass]
  }

instance Show SampleRange where 
  show (SampleRange upBnds tcs) = unwords ["<=", show upBnds, show tcs]