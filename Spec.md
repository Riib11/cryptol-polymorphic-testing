# Spec

The user provides a list of _raw constraints_ defined by the following grammar:
```hs
data RawConstraint
  = Relation Rel Exp Exp
  | Typeclass Tc Int
data Rel = Equ | Leq | Geq
data Exp = Exp [Q] Q
data Tc  = Prime | Fin
```
The list of relation constraints is processed into a system of linear equations,
represented as a matrix over domain `Q`. This system is solved, producing 

The list of raw constraints is processed into _constraints_, defined by the following grammar:
```hs
data Constraints = Constraints 
  { equs :: [Equ]
  , leqs :: [Leq]
  , cones :: [Cone]
  , cocones :: [Cocone]
  , fins :: [Int]
  , primes :: [Int]
  , elimVars :: [Int] -- eliminated variables
  , nVars :: Int
  }
  deriving (Show)

data Equ = Equ Int (Expr Q) deriving (Show) -- xj = a1*x1 + ... + aN*xN + c
data Leq = Leq Int Q deriving (Show) -- xj <= c
data Geq = Geq Int Q deriving (Show) -- c <= xj
data Cone = Cone [Q] Q deriving (Show) -- a1*x1 + ... + aN*xN <= c
data Cocone = Cocone Q [Q] deriving (Show) -- c <= a1*x1 + ... + aN*xN

data Expr a = Expr [a] a deriving (Show) -- a1*x1 + ... + aN*xN + c
```
