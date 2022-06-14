{-# LANGUAGE BlockArguments, LambdaCase #-}

module Solver where

import Control.Monad.Trans
import Control.Monad
import qualified Data.List as List
import qualified Data.Map as Map
import Var
import M
import Q
import Mat
import Constraint
import SolverLib

-- tries to reduce a set of contraints into a set of simple constraints
solve' :: [Constraint] -> M [Constraint]
solve' cons = do
  -- normalize constraints
  cons <- pure $ normConstraint <$> cons

  -- partition contraints into simple and complex
  let (consSimple, consComplex) = partitionConstraints cons
  debug 0 $ "------------------------------------------------------------------"
  debug 0 $ "consSimple = [" ++ List.intercalate ", " (showConstraint <$> consSimple) ++ "]"
  debug 0 $ "consComplex = [" ++ List.intercalate ", " (showConstraint <$> consComplex) ++ "]"

  -- require that there are no variables that are defined by more than one simple equalities
  assertUniqueSimpleConstantEqualities consSimple
          
  (consSimpleEqualities, consSimpleInequalities) <- pure $
    partitionSimpleConstraints consSimple
  debug 0 $ "consSimpleEqualities = " ++ showConstraints consSimpleEqualities
  debug 0 $ "consSimpleInequalities = " ++ showConstraints consSimpleInequalities
  -- substitute all simple equalities into other simple constraints
  consSubstituted <- pure $
    subConstraintByConstraints consSimpleEqualities <$> consSimpleInequalities
  -- combine back in the simple equalities
  consSimple <- pure $ consSimpleInequalities <> consSimpleEqualities
  debug 0 $ "consSimple after substitution: " ++ showConstraints consSimpleEqualities

  -- all simple constraints must be good-formed
  mapM_ assertGoodFormedSimpleConstraint consSimple
  -- if 0 complex constraints, then done
  if length consComplex == 0 then do
    debug 0 $ "finalized solution: " ++ showConstraints consSimple
    pure consSimple
  else do
    -- convert complex constraints into complex equalities
    let eqsComplex = fromComplexConstraintsToComplexEqualities consComplex
    -- normalize complex equalities
    eqsComplex <- pure $ normComplexEquality <$> eqsComplex
    -- convert complex equalities into matrix
    (vars, mat) <- fromComplexEqualitiesToMatrix eqsComplex
    -- gaussElim matrix
    mat <- gaussElim mat
    -- normEchelon matrix 
    mat <- normEchelon mat
    -- convert matrix into equations
    eqs <- fromNormEchelonMatrixToSimpleEqualitys vars mat
    -- add eqs into cons
    cons <- pure $ consSimple <> fmap fromSimpleEqualityToConstraint eqs
    -- recurse on resulting set of constraints
    solve' cons

solve :: [Constraint] -> M (Map.Map Var VarBound, [Constraint])
solve cons = do 
  cons <- solve' cons
  -- enforce integral coefficients
  cons <- constrainIntegralCoefficients cons
  debug 0 $ "cons after enforcing integral coefficients: " ++ showConstraints cons

  -- remove unused vars
  cons <- pure $ pruneUnusedFreshVars cons

  -- (consSimpleEqualities, consSimpleInequalities) <- pure $
  --   partitionSimpleConstraints cons
  -- debug 0 $ "consSimpleEqualities = " ++ showConstraints consSimpleEqualities
  -- debug 0 $ "consSimpleInequalities = " ++ showConstraints consSimpleInequalities
  -- -- substitute all simple equalities into other simple constraints
  -- consSubstituted <- pure $
  --   subConstraintByConstraints consSimpleEqualities <$> consSimpleInequalities
  -- -- combine back in the simple equalities
  -- cons <- pure $ consSimpleInequalities <> consSimpleEqualities
  -- debug 0 $ "cons after substitution: " ++ showConstraints consSimpleEqualities

  -- fvs <- pure $ freeVarsOfConstraints cons
  mvb <- pure $ varBoundsFromConstraints cons

  pure (mvb, cons)

--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--

