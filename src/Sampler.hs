{-# LANGUAGE ScopedTypeVariables #-}

module Sampler where 

import Control.Arrow
import Control.Monad
import qualified Data.Set as Set
import qualified Data.Map as Map 
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
import Solver
import Debug

-- an independent variable is a variable that appears only on the RHS of any safe constraints it appears in.
independents :: Monad m => [SafeConstraint] -> ListT m (Set.Set Symbol)
independents safeCons = do 
  let syms = 
        foldr 
          (\con xs -> case con of 
            SafeRelation x re p -> Set.filter (not . (x ==)) (symbols p <> xs)
            SafeTypeclass x tc -> xs
          )
          Set.empty
          safeCons
  pure syms

-- a dependent variable is a variable that appears on the LHS of at least one safe constraint.
dependents :: Monad m => [SafeConstraint] -> ListT m (Set.Set Symbol)
dependents safeCons = do
  let syms =
        foldr 
          (\con xs -> case con of 
              SafeRelation x re p -> Set.singleton x <> xs
              SafeTypeclass x tc -> xs
          )
          Set.empty
          safeCons
  pure syms

-- an intermediate variable is a variable that appears on the LHS of at least one safe _equality_ constraint and the RHS of at least one safe constraint
intermediates :: Monad m => [SafeConstraint] -> ListT m (Set.Set Symbol)
intermediates safeCons = do
  let
    symsLhs = 
      foldr 
        (\con -> case con of 
          SafeRelation x Eq _ -> Set.insert x
          SafeRelation x _ _ -> id
          SafeTypeclass x _ -> Set.insert x)
        Set.empty
        safeCons
    symsRhs =
      foldr 
        (\con -> case con of 
          SafeRelation _ _ p -> (symbols p <>)
          SafeTypeclass _ _ -> id)
        Set.empty
        safeCons
  pure (Set.intersection symsLhs symsRhs)

-- each Ge is expanded: x <= a[y] ~> x = a[y] + m
expandGes :: Monad m => [SafeConstraint] -> ListT m [SafeConstraint]
expandGes = foldM f []
  where 
    f safeCons (SafeRelation x Ge p) = pure $ 
      SafeRelation x Eq (p <> symbol (genFreshSymbol ())) : safeCons
    f safeCons con = pure (con : safeCons)

-- each Eq is substituted: [x = a[y], ..safeCons] ~> [..safeCons[x := a[y]]]
-- recursively solves if x is on the LHS of any other safeConstraints
substituteIntermediates :: forall m. Monad m => 
  [SafeConstraint] -> ListT m [SafeConstraint]
substituteIntermediates safeCons = do
  inters <- intermediates safeCons
  debugM $ "inters: " ++ show inters

  let
    f :: SafeConstraint -> [SafeConstraint] -> ListT m [SafeConstraint]
    f (SafeRelation x Eq p) safeCons | x `elem` inters = do
      debugM $ "BEGIN substituting: " ++ show x ++ " => " ++ show p ++ " in " ++ show safeCons
      safeCons <- foldM (\safeCons -> ((safeCons <>) <$>)) [] $ sub x p <$> safeCons
      debugM $ "END substituting: " ++ show x ++ " => " ++ show p
      debugM $ "  result: " ++ show safeCons
      pure safeCons

    f safeCon safeCons = pure safeCons

    sub :: Symbol -> Polynomial Z -> SafeConstraint -> ListT m [SafeConstraint]
    sub x p (SafeRelation x' Eq p') | x == x' && p == p' = 
      pure []
    sub x p (SafeRelation x' Eq r) | x == x' = do
      debugM $ "found that " ++ show x ++ " is determined by two equations"
      solve [Relation Eq (fromZ <$> p) (fromZ <$> r)]
    sub x p (SafeRelation y re r) = 
      pure $ [SafeRelation y re (substitute x p r)]
    sub x p (SafeTypeclass y tc) = 
      pure $ [SafeTypeclass y tc]

  foldM (flip f) safeCons safeCons

-- collects safe constraints into sample ranges, where each variable has a unique sample range
toSampleRanges :: Monad m => [SafeConstraint] -> ListT m (Map.Map Symbol SampleRange)
toSampleRanges = foldM f Map.empty
  where
    f m (SafeRelation x Le p) = do
      pure $ Map.alter (Just . g) x m
      where 
        g Nothing = SampleRange [p] []
        g (Just (SampleRange upBnds tcs)) = SampleRange (p : upBnds) tcs
    f m (SafeTypeclass x tc) = do
      pure $ Map.alter (Just . g) x m
      where 
        g Nothing = SampleRange [] [tc]
        g (Just (SampleRange upBnds tcs)) = SampleRange upBnds (tc : tcs)
    f m con = do
      debugM $ "toSampleRange does not handle form: " ++ show con
      ListT.fromFoldable []

-- | samples an assignment of variables that satisfies a given list of safe constraints
sample :: Map.Map Symbol SampleRange -> ListT m Assignment
sample = undefined

-- | Assignment
type Assignment = Map.Map Symbol N
