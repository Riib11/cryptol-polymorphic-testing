{-# Language BlockArguments #-}

module Constraint where

import qualified Data.Set as Set
import qualified Data.List as List
import Data.String (IsString(..))
import System.IO.Unsafe (unsafePerformIO)
import Symbol
import Normal
import Field
import N
import Z
import Q

-- | Constraint

data Constraint cond arith dom
  = Cond (cond (arith dom))
  | Prime Symbol
  | Fin Symbol
  deriving (Eq, Ord)

instance (Show (cond (arith dom)), Show dom) => Show (Constraint cond arith dom) where
  show (Cond cond) = show cond
  show (Prime x) = "prime(" ++ show x ++ ")"
  show (Fin x) = "fin(" ++ show x ++ ")"

-- | Condition

data Condition dom
  = Re Re dom dom
  deriving (Eq, Ord)

instance Show dom => Show (Condition dom) where 
  show (Re r a b) = "(" ++ show a ++ " " ++ show r ++ " " ++ show b ++ ")"

data Re = Eq | Le | Lt | Ge | Gt deriving (Eq, Ord)

instance Show Re where 
  show Eq = "="
  show Le = "<"
  show Lt = "<="
  show Ge = ">"
  show Gt = ">="

-- | Arithmetic

data Arithmetic dom
  = Val (Value dom)
  | Op Op (Arithmetic dom) (Arithmetic dom)
  deriving (Eq)

data Op = Add | Sub | Mul | Div | Mod | Pow deriving (Eq, Ord)

instance Show dom => Show (Arithmetic dom) where
  show (Val (Var s)) = show s
  show (Val (Con c)) = show c
  show (Op o a1 a2) = "(" ++ show a1 ++ " " ++ show o ++ " " ++ show a2 ++ ")"

instance Show Op where 
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Mod = "%"
  show Pow = "^"

instance Symbolic (Arithmetic dom) where 
  symbol s = Val (Var s)
  
  symbols (Val (Var s)) = Set.singleton s
  symbols (Val (Con _)) = mempty
  symbols (Op o a1 a2) = symbols a1 <> symbols a2

instance IsString (Arithmetic dom) where 
  fromString = Val . Var . fromString

instance Eq dom => Ord (Arithmetic dom) where 
  Op _ _ _ <= _ = True
  
  Val (Con _) <= Op _ _ _ = False
  Val (Con _) <= _ = True
  
  Val (Var x) <= Op _ _ _ = False
  Val (Var x) <= Val (Con _) = False
  Val (Var x) <= Val (Var y) = x <= y

instance (Field dom, Num dom) => Num (Arithmetic dom) where
  a + b = a .+ b
  a * b = a .* b
  fromInteger = Val . Con . fromInteger

  abs = undefined
  signum = undefined
  negate = undefined

instance (Ord dom, Field dom) => Normal (Arithmetic dom) where
  simplify (Val _) = Nothing

  -- combine constants
  
  simplify (Op Add (Val (Con c1)) (Val (Con c2))) = Just (Val (Con (c1 .+ c2)))
  simplify (Op Mul (Val (Con c1)) (Val (Con c2))) = Just (Val (Con (c1 .* c2)))
  simplify (Op Div (Val (Con c1)) (Val (Con c2))) = Just (Val (Con (c1 ./ c2)))
  simplify (Op Pow (Val (Con c1)) (Val (Con c2))) = Just (Val (Con (c1 .^ c2)))
  simplify (Op Mod (Val (Con c1)) (Val (Con c2))) = Just (Val (Con (c1 .% c2)))

  -- combine variables

  simplify (Op Add                        (Val (Var s1))                         (Val (Var s2)))  | s1 == s2 = Just (Op Mul (Val (Con (one .+ one))) (Val (Var s1))) -- a + a = 2a
  simplify (Op Add                        (Val (Var s1))  (Op Mul (Val (Con c2)) (Val (Var s2)))) | s1 == s2 = Just (Op Mul (Val (Con (one .+ c2 ))) (Val (Var s1))) -- a + ya = (1 + y)a
  simplify (Op Add (Op Mul (Val (Con c1)) (Val (Var s1)))                        (Val (Var s2)))  | s1 == s2 = Just (Op Mul (Val (Con (c1  .+ one))) (Val (Var s1))) -- xa + a = (x + 1)a
  simplify (Op Add (Op Mul (Val (Con c1)) (Val (Var s1))) (Op Mul (Val (Con c2)) (Val (Var s2)))) | s1 == s2 = Just (Op Mul (Val (Con (c1  .+ c2 ))) (Val (Var s1))) -- xa + ya = (x + y)a

  -- rewrite subtraction
  
  -- a1 - a2 = (0 - a2) + a1
  simplify (Op Sub a1 a2) 
    = Just (Op Add (Op Sub (Val (Con (negative one))) a2) a1)

  -- rewrite division
  
  -- a1/(a2 * a3) = a1 * (1/a1) * (1/a2)
  simplify (Op Div a1 (Op Mul a2 a3))
    = Just (Op Mul (Op Mul a1 (reciprocal a2)) (reciprocal a3))
  
  -- v1/v2 normal
  simplify (Op Div (Val _) (Val _)) = Nothing
  
  -- a/v = (1/v) * a
  simplify (Op Div a (Val v)) 
    = Just (Op Mul (reciprocal (Val v)) a)
  
  -- associativity
  
  -- a1 • (a2 • a3) = (a1 • a2) • a3
  simplify (Op o a1 (Op o' a2 a3)) | o `elem` [Add, Mul] && o == o'
    = Just (Op o (Op o' a1 a2) a3)
  
  -- commutativity
  
  -- a1 • a2 = a2 • a1
  simplify (Op o a1 a2) | o `elem` [Add, Mul] && a1 > a2 
    = Just (Op o a2 a1)
  
  -- (a1 • a2) • a3 = (a1 • a3) • a2
  simplify (Op o (Op o' a1 a2) a3) | o `elem` [Add, Mul] && o == o' && a2 > a3 
    = Just (Op o (Op o' a1 a3) a2)

  -- distributivity
  
  -- a1 * (a2 + a3) = (a1 * a2) + (a1 * a3)
  simplify (Op Mul a1 (Op Add a2 a3)) 
    = Just (Op Add (Op Mul a1 a2) (Op Mul a1 a3))

  -- traversal

  simplify (Op o a1 a2) =
    case simplify a1 of 
      Just a1' -> Just (Op o a1' a2)
      Nothing -> case simplify a2 of
        Just a2' -> Just (Op o a1 a2')
        Nothing -> Nothing

  -- otherwise, is already normal

  -- TODO: turns out this pattern is redundant? I thought it wouldn't be...
  -- simplify _ = Nothing

-- | Value

data Value dom
  = Var Symbol
  | Con dom
  deriving (Eq, Ord)

instance Symbolic (Value dom) where 
  symbol s = Var s

  symbols (Var s) = Set.singleton s
  symbols (Con _) = mempty

instance Field dom => Field (Arithmetic dom) where 
  zero = Val (Con zero)
  one = Val (Con one)

  a1 .+ a2 = Op Add a1 a2 
  a1 .- a2 = Op Sub a1 a2
  a1 .* a2 = Op Mul a1 a2
  a1 ./ a2 = Op Div a1 a2
  a1 .^ a2 = Op Pow a1 a2
  a1 .% a2 = Op Mod a1 a2