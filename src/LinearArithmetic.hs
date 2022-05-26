{-# Language OverloadedStrings #-}

module LinearArithmetic where

import qualified Data.List as List
import Data.String (IsString(..))

-- | Data

data Constraint 
  = Prime Symbol
  | Fin Symbol
  | Cond BExp
  deriving (Eq)

data AExp
  = Var Symbol
  | Const Int -- x >= 0
  | Op2 Op2 AExp AExp
  deriving (Eq)

instance Num AExp where
  (+) = Op2 Add
  (*) = Op2 Add
  abs = id
  signum = undefined
  negate = undefined
  fromInteger = Const . fromInteger

instance Fractional AExp where 
  (/) = Op2 Div
  fromRational = undefined

instance IsString AExp where 
  fromString str = Var (fromString str)

instance Ord AExp where
  Const _ <= _ = True
  
  Var s <= Const _ = False
  Var s1 <= Var s2 = s1 <= s2
  Var s <= _ = True

  -- Op2 o a1 a2 <= a3 = (a1 <= a3) && (a2 <= a3)
  -- Op2 o a1 a2 <= Const _ = False
  -- Op2 o a1 a2 <= Var _ = False
  -- Op2 o1 a1 a2 <= Op2 o2 a3 a4 = o1 <= o2 && 
  _ <= _ = True -- TODO: should handle Op2 in some way?

data Op2 = Add | Sub | Mul | Div deriving (Eq)

interpOp2 :: Op2 -> (Int -> Int -> Int)
interpOp2 Add = (+)
interpOp2 Sub = (-)
interpOp2 Mul = (*)
interpOp2 Div = div

data BExp = Re2 Re2 AExp AExp deriving (Eq)

data Re2 = Eq | Lt | Le | Gt | Ge deriving (Eq)

newtype Symbol = Symbol String deriving (Eq, Ord)

freshVar :: () -> AExp
freshVar _ = Var undefined

instance IsString Symbol where 
  fromString str = Symbol str

-- | Normalize instances

class Normalize a where
  -- `simplify a = Just a'` if `a` can by simplified into `a'` by a single simplification
  -- `simplify a = Nothing` if `a` is in normal form
  {-# MINIMAL simplify #-}
  simplify :: a -> Maybe a
  
  normalize :: a -> a
  normalize a = case simplify a of 
    Just a' -> normalize a'
    Nothing -> a

instance Normalize BExp where 
  -- normal form: a1 = a2
  simplify (Re2 Eq _  _ ) = Nothing
  simplify (Re2 Lt a1 a2) = Just (Re2 Eq (1 + a1 + freshVar ()) a2)
  simplify (Re2 Le a1 a2) = Just (Re2 Eq (a1 + freshVar ()) a2)
  simplify (Re2 Gt a1 a2) = Just (Re2 Lt a2 a2)
  simplify (Re2 Ge a1 a2) = Just (Re2 Le a2 a2)

instance Normalize AExp where
  -- normal form: -- TODO

  -- constants
  -- can assume that this is always valid, since SMT solver will make sure we never go negative
  simplify (Op2 Add 0 a2) = Just a2
  simplify (Op2 Sub a (Const x)) = Just (Const (- x) + a)
  simplify (Op2 Mul 0 a2) = Just 0
  simplify (Op2 Mul 1 a2) = Just a2
  simplify (Op2 Div 1 a2) = Just a2
  simplify (Op2 o (Const x) (Const y)) = Just (Const (interpOp2 o x y))

  -- commutativity
  simplify (Op2 Add x y) | x > y = Just (y + x)
  simplify (Op2 Add x (Op2 Add y z)) | x > y = Just (y + (x + z))
  simplify (Op2 Mul x y) | x > y = Just (y * x)
  simplify (Op2 Mul x (Op2 Add y z)) | x > y = Just (y * (x * z))

  -- multi-subtraction
  simplify (Op2 Sub (Op2 Sub a1 a2) a3) = Just (a1 - (a2 + a3)) -- (a1 - a2) - a3 = a1 - (a2 + a3)
  simplify (Op2 Sub a1 (Op2 Sub a2 a3)) = Just (a1 + (a3 + a2)) -- a1 - (a2 - a3) = a1 + (a3 - a2)

  -- multi-division
  simplify (Op2 Div (Op2 Div a1 a2) a3) = Just (a1 / (a2 * a3)) -- (a1 / a2) / a3 = a1 / (a2 * a3)
  simplify (Op2 Div a1 (Op2 Div a2 a3)) = Just ((a1 * a3) / a2) -- a1 / (a2 / a3) = (a1 * a3) / a2

  -- associativity
  simplify (Op2 Add (Op2 Add a1 a2) a3) = Just (a1 + (a2 + a3))
  simplify (Op2 Mul (Op2 Mul a1 a2) a3) = Just (a1 * (a2 * a3))

  -- distributivity
  simplify (Op2 Mul a1 (Op2 Add a2 a3)) = Just ((a1 * a2) + (a1 * a3))

  -- group variables
  simplify (Op2 Add (Var s1) (Var s2)) | s1 == s2 = Just (2 * Var s1) -- a + a = 2a
  simplify (Op2 Add (Var s1) (Op2 Mul (Const y) (Var s2))) | s1 == s2 = Just ((1 + Const y) * Var s1) -- a + ya = (1 + y)a
  simplify (Op2 Add (Op2 Mul (Const x) (Var s1)) (Var s2)) | s1 == s2 = Just ((Const x + 1) * Var s1) -- xa + a = (x + 1)a
  simplify (Op2 Add (Op2 Mul (Const x) (Var s1)) (Op2 Mul (Const y) (Var s2))) | s1 == s2 = Just ((Const x + Const y) * Var s1) -- xa + ya = (x + y)a

  -- traverse
  simplify (Op2 o a1 a2) =
    case simplify a1 of 
      Just a1' -> Just (Op2 o a1' a2)
      Nothing -> case simplify a2 of
        Just a2' -> Just (Op2 o a1 a2')
        Nothing -> Nothing

  -- done
  simplify a = Nothing

-- Analyzing variables

class FreeVars a where 
  freeVars :: a -> [Symbol]

instance FreeVars Constraint where 
  freeVars = undefined

instance FreeVars BExp where 
  freeVars = undefined

instance FreeVars AExp where 
  freeVars = undefined

appearsIn :: FreeVars a => Symbol -> a -> Bool 
appearsIn s a = s `elem` freeVars a

-- `solve x (f(x,ys) ~ g(x,ys)) = (h(ys), conds)` such that `x = h(ys, zs)`
-- where 
-- • `ys` are all the free vars other than `xs` in the original condition,
-- • `zs` are fresh variables constrained by `conds` introduced to ensure that 
--   inversions of `+`, `*` are handled properly
-- requires: b is in normal form
-- requires: b is a BExp such that `x` appears exactly once
solve :: Symbol -> BExp -> BExp
solve x (Re2 Eq a1 a2) | x `appearsIn` a2 = solve x (Re2 Eq a2 a1)
solve x (Re2 Eq a1 a2) | x `appearsIn` a1 = undefined
  where
    vars = List.nub (freeVars a1 ++ freeVars a2)
    ys = List.delete x vars
    zs = filter (not . (`elem` ys)) vars_a3

    f_inv = invertAExpAtVar x a1 -- invert `a1[x]` at `x`
    a3 = f_inv a2
    vars_a3 = freeVars a3
    
-- invertAExprAtVar x a1[x] a2 = 
invertAExpAtVar :: Symbol -> AExp -> AExp -> AExp
invertAExpAtVar x a1 a2 | x `appearsIn` a1 = go a1 a2
  where
    go :: AExp -> AExp -> AExp
    
    go (Var y) a2 | x == y = a2
    
    go (Op2 Add a1 a2) a3 | x `appearsIn` a1 = go a1 (a3 - a2) -- a1[x] + a2 = a3 --> a1[x] = a3 - a2
    go (Op2 Add a1 a2) a3 | x `appearsIn` a2 = go (a2 + a1) a3 -- flip via add_comm
    
    go (Op2 Sub a1 a2) a3 | x `appearsIn` a1 = go a1 (a3 + a2) -- a1[x] - a2 = a3 --> a1[x] = a3 + a2
    go (Op2 Sub a1 a2) a3 | x `appearsIn` a2 = go a2 (a1 - a3) -- a1 - a2[x] = a3 --> a2[x] = a1 - a3
    
    go (Op2 Mul a1 a2) a3 | x `appearsIn` a1 = go a1 (a3 / a2) -- a1[x] * a2 = a3 --> a1[x] = a3 / a2
    go (Op2 Mul a1 a2) a3 | x `appearsIn` a2 = go (a2 * a1) a3 -- flip via mul_comm
    
    go (Op2 Div a1 a2) a3 | x `appearsIn` a1 = go a1 (a3 * a2) -- a1[x] / a2 = a3 --> a1[x] = a3 * a2
    go (Op2 Div a1 a2) a3 | x `appearsIn` a2 = go a2 (a1 / a3) -- a1 / a2[x] = a3 --> a1[x] = a1 / a3
    
    go a1 a2 = error $
      unlines 
        [ unwords ["solve", show x, show (Re2 Eq a1 a2)]
        , "a1 = " ++ show a1
        , "a2 = " ++ show a2 ]

-- | Show instances

instance Show Constraint where 
  show (Prime s) = "prime(" ++ show s ++ ")"
  show (Fin s) = "fin(" ++ show s ++ ")"
  show (Cond b) = show b

instance Show AExp where 
  show (Var s) = show s 
  show (Const x) = show x
  show (Op2 o x y) = "(" ++ show x ++ " " ++ show o ++ " " ++ show y ++ ")"

instance Show Op2 where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

instance Show BExp where 
  show (Re2 r x y) = show x ++ " " ++ show r ++ " " ++ show y

instance Show Re2 where 
  show Eq = "="
  show Lt = "<"
  show Le = "<="
  show Gt = ">"
  show Ge = ">="

instance Show Symbol where 
  show (Symbol str) = str