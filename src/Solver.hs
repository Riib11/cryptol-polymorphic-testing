{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiWayIf, LambdaCase, GeneralizedNewtypeDeriving #-}

module Solver where

import Control.Arrow
import Control.Monad
import qualified Data.Set as Set
import Data.Maybe
import Symbol
import Normal
import N
import Z
import Q
import LinearArithmetic.Polynomial as Polynomial
import Nondet

-- | Constraint
data Constraint a
  = Relation Re (Polynomial a) (Polynomial a) -- a + sum(bi * xi) ~ c + sum(dj * yj)
  | Typeclass Typeclass

instance (Show a, Num a, Eq a) => Show (Constraint a) where
  show (Relation re p1 p2) = concat ["(", show p1, show re, show p2, ")"]
  show (Typeclass tc) = show tc

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
data Typeclass
  = Prime Symbol
  | Fin Symbol

instance Show Typeclass where 
  show (Prime x) = "(prime " ++ show x ++ ")"
  show (Fin x) = "(fin" ++ show x ++ ")"

-- | SolvedConstraint
data SolvedConstraint a 
  = SolvedRelation Symbol Re (Polynomial a) -- a ~ b + sum(ci * xi)
  | SolvedTypeclass Typeclass

instance (Show a, Num a, Eq a) => Show (SolvedConstraint a) where
  show (SolvedRelation x re p) = 
    "solved" ++ unwords ["(", show x, show re, show p, ")"]
  show (SolvedTypeclass tc) = "solved" ++ show tc

-- solves each relation for one variable to put on the LHS
toSolvedConstraintQ :: Constraint Q -> Nondet (SolvedConstraint Q)
toSolvedConstraintQ (Relation re lhs rhs) = do
  -- move everything to LHS
  let lhs' = lhs <> (negate <$> rhs)
  -- choose a variable to solve for
  let vars = symbols lhs'
  x <- choices (Set.toList vars)
  -- solve for the chosen variable
  let rhs' = 
        foldr (>>>) id 
          [ delete x -- keep x on LHS
          , fmap negate -- move everything else to RHS
          , fmap (/ coefficient x lhs') -- divide by coefficient of x
          ]
          lhs'
  -- flip relation if divided by a negative
  let re' = flipRe re
  --
  pure $ SolvedRelation x re' rhs'
toSolvedConstraintQ (Typeclass tc) =
  pure $ SolvedTypeclass tc

-- tries to round numbers from Q to N
toSolvedConstraintZ :: SolvedConstraint Q -> Nondet (SolvedConstraint Z)

toSolvedConstraintZ (SolvedRelation x Eq rhs) = do
  -- equalities with an RHS that has non-integer coefficients are discarded
  guard $ all (isJust . toZ) (coefficients rhs)
  pure $ SolvedRelation x Eq (fromJust . toZ <$> rhs)
-- 
toSolvedConstraintZ (SolvedRelation x Le rhs) =
  pure $ SolvedRelation x Eq (roundDown <$> rhs)
toSolvedConstraintZ (SolvedRelation x Ge rhs) =
  pure $ SolvedRelation x Eq (roundUp <$> rhs)
-- 
toSolvedConstraintZ (SolvedTypeclass tc) = pure $ SolvedTypeclass tc

-- | SafeConstraint

data SafeConstraint
  = SafeRelation Symbol Re (Polynomial Z)
  | SafeTypeclass Typeclass

instance Show SafeConstraint where
  show (SafeRelation x re p) =
    "safe" ++ concat ["(", show x, show re, show p, ")"]
  show (SafeTypeclass tc) = "safe" ++ show tc

-- if a term in the RHS has a negative coeffcient, then need to make sure that the amount being subtracted is less than the amount being subtracted from
toSafeConstraints :: SolvedConstraint Z -> Nondet [SafeConstraint]
toSafeConstraints (SolvedRelation x re rhs) =
  let
    c = constant rhs -- constant
    ptvs = Polynomial.filter (>= 0) rhs -- positif terms (i.e. non-negative)
    negs = Polynomial.filter (< 0) rhs -- negative terms
  in
    if
      | -- x = c
        Polynomial.size ptvs == 0 && Polynomial.size negs == 0
      -> pure []
      | -- x = sum(ai*yi) + c
        Polynomial.size ptvs >= 1 && Polynomial.size negs == 0 
      -> pure [SafeRelation x re rhs]
      | -- x = sum(ai*yi) + c - sum(bj*zj)
        Polynomial.size ptvs >= 1 && Polynomial.size negs >= 1
      -> do
        -- sum(bj*zj) <= sum(ai*yi) + c
        -- TODO: make sure no infinite loops here, since recursive
        safeCons <- solve 
          [ Relation Le 
              (fromZ . abs <$> negs)
              ((fromZ <$> ptvs) <> (fromZ <$> Polynomial.singleton c)) ]
        pure $ SafeRelation x re rhs : safeCons
toSafeConstraints (SolvedTypeclass tc) = 
  pure [SafeTypeclass tc]

-- | solve
-- TODO: test on some complicated expressions

solve :: [Constraint Q] -> Nondet [SafeConstraint]
solve cons = do 
  solConQs <- sequence $ toSolvedConstraintQ <$> cons
  solConZs <- sequence $ toSolvedConstraintZ <$> solConQs
  safeCons <- fmap mconcat . sequence $ toSafeConstraints <$> solConZs
  pure safeCons
