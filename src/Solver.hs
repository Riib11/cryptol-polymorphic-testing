{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiWayIf, LambdaCase, GeneralizedNewtypeDeriving #-}

module Solver where

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
import ListT
import Constraint
import Debug

-- | solves each relation for one variable to put on the LHS
toSolvedConstraintQ :: Monad m => Constraint Q -> ListT m (SolvedConstraint Q)
toSolvedConstraintQ (Relation re lhs rhs) = do
  -- move everything to LHS
  let lhs' = lhs <> (negate <$> rhs)
  -- choose a variable to solve for
  let vars = symbols lhs'
  x <- fromFoldable $ Set.toList vars
  let a = coefficient x lhs' -- coefficient of x
  -- solve for the chosen variable
  let rhs' = 
        foldr (>>>) id 
          [ delete x -- keep x on LHS
          , fmap negate -- move everything else to RHS
          , fmap (/ a) -- divide by coefficient of x
          ]
          lhs'
  -- flip relation if divided by a negative
  let re' = if a >= 0 then re else flipRe re
  --
  pure $ SolvedRelation x re' rhs'
toSolvedConstraintQ (Typeclass x tc) =
  pure $ SolvedTypeclass x tc

-- | tries to round numbers from Q to N
toSolvedConstraintZ :: Monad m => SolvedConstraint Q -> ListT m (SolvedConstraint Z)

toSolvedConstraintZ (SolvedRelation x Eq rhs) = do
  -- equalities with an RHS that has non-integer coefficients are discarded
  if all (isJust . toZ) (coefficients rhs) then
    pure $ SolvedRelation x Eq (fromJust . toZ <$> rhs)
  else do
    debugM $ "Could not convert the solved relation \"" ++ show (SolvedRelation x Eq rhs) ++ "\" from domain Q to domain Z."
    debugM ""
    empty
-- 
toSolvedConstraintZ (SolvedRelation x Le rhs) =
  pure $ SolvedRelation x Le (roundDown <$> rhs)
toSolvedConstraintZ (SolvedRelation x Ge rhs) =
  pure $ SolvedRelation x Ge (roundUp <$> rhs)
-- 
toSolvedConstraintZ (SolvedTypeclass x tc) = pure $ SolvedTypeclass x tc


-- | if a term in the RHS has a negative coeffcient, then need to make sure that the amount being subtracted is less than the amount being subtracted from
toSafeConstraints :: Monad m => SolvedConstraint Z -> ListT m [SafeConstraint]
toSafeConstraints (SolvedRelation x re rhs0) =
  let
    -- make c >= 0
    rhs1 = rhs0 -- if constant rhs0 >= 0 then rhs0 else negate <$> rhs0
    [ptvs, negs] = [P.filter (>= 0) rhs1, P.filter (< 0) rhs1]
    c = constant rhs1
  in
    if
      | -- x ~ c
        P.size ptvs == 0 && P.size negs == 0
      -> pure [SafeRelation x re rhs1]
      | -- x ~ sum(ai*yi) + c
        P.size ptvs >= 1 && P.size negs == 0 
      ->
        if c >= 0 then 
          pure [SafeRelation x re rhs1]
        else do
          let cons = [ Relation Ge 
                        (fromZ <$> ptvs)
                        (fromZ <$> P.singleton (negate c)) ]
          debugM $ "Additional constraints: " ++ show cons
          safeCons <- solve cons
          pure $  SafeRelation x re rhs1 : safeCons
      | -- x ~ sum(ai*yi) + c - sum(bj*zj)
        P.size ptvs >= 1 && P.size negs >= 1
      -> do
        -- sum(bj*zj) <= sum(ai*yi) + c
        -- TODO: make sure no infinite loops here, since recursive
        let cons = [ Relation Le 
                      (fromZ . abs <$> negs)
                      ((fromZ <$> ptvs) <> (fromZ <$> P.singleton c)) ]
        debugM $ "Additional constraints: " ++ show cons
        safeCons <- solve cons
        pure $ SafeRelation x re rhs1 : safeCons
      | otherwise -> do
          debugM $ "Could not convert the solved relation \"" ++ show (SolvedRelation x re rhs1) ++ "\" to a safe constraint."
          debugM $ "ptvs = " ++ show ptvs
          debugM $ "negs = " ++ show negs
          debugM $ "c    = " ++ show c
          debugM ""
          empty
toSafeConstraints (SolvedTypeclass x tc) = 
  pure [SafeTypeclass x tc]

-- | solve
-- TODO: test on some complicated expressions

solve :: Monad m => [Constraint Q] -> ListT m [SafeConstraint]
solve cons = do 
  debugM $ "BEGIN solve: " ++ show cons
  solConQs <- sequence $ toSolvedConstraintQ <$> cons
  debugM $ "solConQs: " ++ show solConQs
  solConZs <- sequence $ toSolvedConstraintZ <$> solConQs
  debugM $ "solConZs: " ++ show solConZs
  safeCons <- fmap mconcat . sequence $ toSafeConstraints <$> solConZs
  debugM $ "safeCons: " ++ show safeCons
  debugM $ "END solve: " ++ show cons
  pure safeCons
