{-# Language OverloadedStrings #-}
{-# Language BlockArguments #-}

module LinearArithmetic where

import GHC.Real
import qualified Data.List as List
import Data.String (IsString(..))
import System.IO.Unsafe (unsafePerformIO)

-- | Data

data Constraint 
  = Prime Symbol
  | Fin Symbol
  | Cond BExp
  deriving (Eq)

data AExp 
  = Tm Rational (Maybe Symbol) -- c * x
  | Op2 Op2 AExp AExp -- a1 • a2
  deriving (Eq)

instance Num AExp where
  (+) = Op2 Add
  (*) = Op2 Add
  abs = id
  signum = undefined
  
  negate (Tm c x) = Tm (negate c) x
  negate (Op2 Add a1 a2) = Op2 Add (negate a1) (negate a2)
  negate (Op2 Sub a1 a2) = Op2 Sub a2 (negate a1)
  negate (Op2 Mul a1 a2) = Op2 Mul (negate a1) a2
  negate (Op2 Div a1 a2) = Op2 Div (negate a1) a2

  fromInteger = (\c -> Tm c Nothing) . fromInteger

instance Fractional AExp where 
  (/) = Op2 Div
  fromRational = undefined

instance IsString AExp where 
  fromString str = Tm 1 (Just (fromString str))

instance Ord AExp where
  -- Tm _ Nothing <= _ = True

  -- Tm _ (Just _) <= Tm _ Nothing = False
  -- Tm c1 (Just x1) <= Tm c2 (Just x2) = x1 <= x2
  -- Tm _ _ <= Op2 _ _ _ = True

  -- Op2 _ _ _ <= Tm _ _ = False
  -- Op2 o1 a1 a2 <= _ = True

  Op2 _ _ _ <= _ = True

  Tm _ _ <= Op2 _ _ _ = False

  Tm _ Nothing <= Tm _ _ = True
  
  Tm _ (Just _) <= Tm _ Nothing = False
  Tm _ (Just x1) <= Tm _ (Just x2) = x1 <= x2

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
freshVar _ = Tm 1 undefined

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

  normalize' :: Int -> a -> Either a a
  normalize' g a | g > 0 = case simplify a of 
    Just a' -> normalize' (g - 1) a'
    Nothing -> pure a
  normalize' g a | g <= 0 = Left a

instance Normalize BExp where 
  -- normal form: a1 = a2
  simplify (Re2 Eq _  _ ) = Nothing
  simplify (Re2 Lt a1 a2) = Just (Re2 Eq (1 + a1 + freshVar ()) a2)
  simplify (Re2 Le a1 a2) = Just (Re2 Eq (a1 + freshVar ()) a2)
  simplify (Re2 Gt a1 a2) = Just (Re2 Lt a2 a2)
  simplify (Re2 Ge a1 a2) = Just (Re2 Le a2 a2)

instance Normalize AExp where
  -- normal form: -- TODO define normal form

  -- rewrite subtraction
  simplify (Op2 Sub a1 a2) = Just (Op2 Add a1 (negate a2))
  
  -- rewrite division
  simplify (Op2 Div a1 (Tm c x)) = Just (Op2 Mul a1 (Tm (recip c) x))
  simplify (Op2 Div a1 a2) = Just (Op2 Mul a1 (recip a2)) -- TODO: still causes issues if you have multiple variables in divisor

  -- associativity
  -- simplify (Op2 Add (Op2 Add a1 a2) a3) = Just (a1 + (a2 + a3))
  -- simplify (Op2 Mul (Op2 Mul a1 a2) a3) = Just (a1 * (a2 * a3))
  simplify (Op2 Add a1 (Op2 Add a2 a3)) = Just ((a1 + a2) + a3)
  simplify (Op2 Mul a1 (Op2 Mul a2 a3)) = Just ((a1 * a2) * a3)

  -- commutativity
  -- ensures that constant terms are first, followed by variale terms in order
  simplify (Op2 Add a1 a2) | a1 > a2 = Just (a2 + a1)
  simplify (Op2 Add (Op2 Add a1 a2) a3) | a2 > a3 = Just ((a1 + a3) + a2)
  simplify (Op2 Mul a1 a2) | a1 > a2 = Just (a2 * a1)
  simplify (Op2 Mul (Op2 Mul a1 a2) a3) | a2 > a3 = Just ((a1 * a3) * a2)

  -- distributivity
  simplify (Op2 Mul a1 (Op2 Add a2 a3)) = Just ((a1 * a2) + (a1 * a3))

  -- combine constant terms
  simplify (Op2 Add (Tm c1 Nothing) (Tm c2 Nothing)) = Just (Tm (c1 + c2) Nothing)
  simplify (Op2 Mul (Tm c1 Nothing) (Tm c2 Nothing)) = Just (Tm (c1 * c2) Nothing)

  -- eliminate identities
  simplify (Op2 Add 0 a2) = Just a2
  simplify (Op2 Mul 0 a2) = Just 0
  simplify (Op2 Mul 1 a2) = Just a2

  -- combine constant factors
  simplify (Op2 Mul (Tm c1 Nothing) (Tm c2 (Just x2))) = Just (Tm (c1 * c2) (Just x2))


  -- combine variable terms
  simplify (Op2 Add (Tm c1 (Just x1)) (Tm c2 (Just x2))) | x1 == x2 = Just (Tm (c1 + c2) (Just x1))
  simplify (Op2 Mul (Tm c1 (Just x1)) (Tm c2 (Just x2))) | x1 == x2 = Just (Tm (c1 * c2) (Just x1))

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

{- -- TODO
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
-}

-- | Show instances

instance Show Constraint where 
  show (Prime s) = "prime(" ++ show s ++ ")"
  show (Fin s) = "fin(" ++ show s ++ ")"
  show (Cond b) = show b

instance Show AExp where
  show (Tm c Nothing) = showRational c
  show (Tm c (Just x)) = showRational c ++ show x
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

showRational :: Rational -> String
showRational (1 :% 1) = "1"
showRational (n :% 1) = show n
showRational (n :% d) = "(" ++ show n ++ "/" ++ show d ++ ")"