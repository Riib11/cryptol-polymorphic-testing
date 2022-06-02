{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiWayIf, LambdaCase, GeneralizedNewtypeDeriving #-}

module Solver where

import Control.Arrow
import qualified Data.Set as Set
import qualified Data.Map as Map
import M
import Graph
import PolyGraph
import Symbol
import N
import Z
import Q
import Field
import Normal
import LinearArithmetic
import EqualityCondition
import Constraint

-- | DependencyGraph

type DependencyGraph = PolyGraph Symbol Dependency

data Dependency
  = Dependency_EqualityCondition (EqualityCondition (LinearArithmetic Q))
  | Dependency_SolvedDependency SolvedDependency

instance Show Dependency where
  show (Dependency_EqualityCondition eq) = show eq
  show (Dependency_SolvedDependency solDep) = show solDep

toDependencyGraph :: [Constraint EqualityCondition LinearArithmetic Q] -> M DependencyGraph
toDependencyGraph = pure . foldr f mempty
  where 
    f (Cond (Equal a1 a2)) = PolyGraph.addEdge (symbols a1 <> symbols a2, Dependency_EqualityCondition (Equal a1 a2))
    f (Prime x) = PolyGraph.addEdge (Set.singleton x, Dependency_SolvedDependency (SolvedPrime x))
    f (Fin x) = PolyGraph.addEdge (Set.singleton x, Dependency_SolvedDependency (SolvedFin x))

-- | NormalizedDependencyGraph

type NormalizedDependencyGraph = PolyGraph Symbol NormalizedDependency

data NormalizedDependency 
  = NormalizedDependency_LinearArithmeticEquation (LinearArithmeticEquation Q)
  | NormalizedDependency_SolvedDependency SolvedDependency

instance Show NormalizedDependency where 
  show (NormalizedDependency_LinearArithmeticEquation eq) = show eq
  show (NormalizedDependency_SolvedDependency dep) = show dep 

-- encodes f[x1, ..., xN] = 0, where f is a linear arithmetic expression
newtype LinearArithmeticEquation dom = LinearArithmeticEquation (LinearArithmetic dom) deriving (Eq, Functor)

instance Show dom => Show (LinearArithmeticEquation dom) where
  show (LinearArithmeticEquation a) = show a ++ " = 0"

toNormalizedDependencyGraph :: DependencyGraph -> M NormalizedDependencyGraph
toNormalizedDependencyGraph dg = do
  logM "toNormalizedDependencyGraph" (show dg)
  PolyGraph.liftOutEdge $ fmap f dg
  where
    f (Dependency_EqualityCondition eq) = NormalizedDependency_LinearArithmeticEquation <$> toLinearArithmeticEquation eq
    f (Dependency_SolvedDependency sd) = pure $ NormalizedDependency_SolvedDependency sd

toLinearArithmeticEquation :: EqualityCondition (LinearArithmetic Q) -> M (LinearArithmeticEquation Q)
toLinearArithmeticEquation eq@(Equal a1 a2) = do 
  logM "toLinearArithmeticEquation [input]" (show eq)
  eq <- a1 `subLA` a2
  logM "toLinearArithmeticEquation [ouput]" (show eq)
  pure $ LinearArithmeticEquation eq

-- | SolvedDependencyGraph

-- x --{y = f[x]}--> y, where f is a linear arithmetic expression
type SolvedDependencyGraph = Graph Symbol SolvedDependency

-- | SolvedDependency

data SolvedDependency
  = SolvedAssignment Symbol (LinearArithmetic N) -- x = f[y1, ..., yN], where f is an linear arithmetic expression
  | SolvedUpperBound Symbol N
  | SolvedLowerBound Symbol N
  | SolvedPrime Symbol -- prime x
  | SolvedFin Symbol -- fin x

instance Show SolvedDependency where
  show (SolvedAssignment x a) = show x ++ " := " ++ show a
  show (SolvedUpperBound x n) = show x ++ " :<= " ++ show n
  show (SolvedLowerBound x n) = show x ++ " :>= " ++ show n
  show (SolvedPrime x) = "prime(" ++ show x ++ ")"
  show (SolvedFin x) = "fin(" ++ show x ++ ")"

toSolvedDependencyGraph :: NormalizedDependencyGraph -> M SolvedDependencyGraph
toSolvedDependencyGraph ndg =
  foldr
    (\(vs, nd) m_ndg -> case nd of 
      NormalizedDependency_LinearArithmeticEquation eq -> 
        caseM (toSolvedDependencies eq) >>= \case
          Left msg -> throwM msg
          Right solDeps ->
            foldr
              (\solDep m_ndg -> case solDep of 
                SolvedAssignment x a -> 
                    (foldr (>>>) id 
                      [ Graph.addEdge (x, y, solDep) | y <- Set.toList (symbolsLinearArithmetic a) ]) 
                    <$> m_ndg
                SolvedUpperBound x n -> fmap (Graph.addEdge (x, x, SolvedUpperBound x n)) m_ndg
                SolvedLowerBound x n -> fmap (Graph.addEdge (x, x, SolvedLowerBound x n)) m_ndg
                SolvedPrime x -> fmap (Graph.addEdge (x, x, SolvedPrime x)) m_ndg
                SolvedFin x -> fmap (Graph.addEdge (x, x, SolvedFin x)) m_ndg
              )
              m_ndg
              solDeps
      NormalizedDependency_SolvedDependency (SolvedPrime x) -> fmap (Graph.addEdge (x, x, SolvedPrime x)) m_ndg
      NormalizedDependency_SolvedDependency (SolvedFin x) -> fmap (Graph.addEdge (x, x, SolvedFin x)) m_ndg
    )
    (pure mempty)
    (PolyGraph.edges ndg)

-- eliminate all denomenators
-- partition by positive/negative terms
-- check if in a simple form:
-- • exactly 1 positive term
-- • exactly 1 negative term
toSolvedDependencies :: LinearArithmeticEquation Q -> M [SolvedDependency]
toSolvedDependencies eq0@(LinearArithmeticEquation (Sum c0 tms0)) = 
  let 
    -- eliminate denomenators    
    ns = c0 : coefficients tms0 -- constant and coefficients
    dens = denomenator <$> ns -- denomenators
    lcm_dens = lcmList dens -- lcm of denomenators
    eliminate_denomenator (Q a b) = a * fromN lcm_dens
    eq1@(LinearArithmeticEquation (Sum c1 tms1)) = eliminate_denomenator <$> eq0

    -- ensure constant is negative
    eq2@(LinearArithmeticEquation (Sum c2 tms2)) = if c1 > 0 then negate <$> eq1 else eq1
    -- 
    c = toN $ negate c2

    -- partition terms into those with positive/negative coefficients
    -- positives = map (\(s, c) -> (s, toN c)) .  filter (\(s, c) -> c >= 0) $ tmsList2
    tmsPos = fmap toN . overTerms (Map.filter (>= 0)) $ tms2
    -- negatives = map (\(s, c) -> (s, toN c)) . filter (\(s, c) -> c <  0) $ tmsList2
    tmsNeg = fmap toN . overTerms (Map.filter (< 0)) $ tms2
    
    -- result is in the form:
    -- sum(ai*xi) - sum(bj*yj) == c
    -- where 
    -- • sum(ai*xi) is the sum of positive terms
    -- • - sum(bj*yj) is the sum of negatve terms
  in 
    -- handle specific forms
    if
      -- a*x = c
      | [(x, a)] <- toTermsList tmsPos, [] <- toTermsList tmsNeg -> 
        -- TODO: figure out which way to round c
        undefined
      -- -a*x = c
      | [] <- toTermsList tmsPos, [(a, x)] <- toTermsList tmsNeg ->
        -- TODO: figure out which way to round c
        undefined
      -- a*x = c + sum(bj*yj)
      | [(x, a)] <- toTermsList tmsPos ->
        let 
          negativeQuotRems = (`quotRem` a) <$> tmsNeg
        in
          -- check that each bj can be divided by a
          if all ((== 0) . snd) negativeQuotRems then 
            let
              c' = undefined -- TODO: figure out which way to round this
              tmsNeg' = fst <$> negativeQuotRems
              -- result is in the form:
              -- x = c + sum(bi + yi) where c, bi : N
            in
              pure [SolvedAssignment x (Sum c' tmsNeg')]
          else
            throwM $ "the RHS of `LinearArithmeticEquation Q` cannot be divided by the coefficient of variable being solved for in the following: " ++ show eq2
      -- TODO: what other forms are interesting?
      | otherwise -> throwM $ "the `LinearArithmeticEquation Q` is not of a form that can be solved: " ++ show eq2

-- | Solution

newtype Solution = Solution [SolvedDependency] deriving (Show)

-- | Solver algorithm:
-- 1. LinearConstraint
-- 2. DependencyGraph
-- 3. NormalizedDependencyGraph
-- 4. SolvedDependencyGraph
-- 5. Solution
solve :: [Constraint Condition Arithmetic Q] -> M Solution
solve cons = do
  linCons <- fmap toLinearConstraint >>> sequence $ cons
  logM "linCons" $ unwords (show <$> linCons)

  depGraph <- toDependencyGraph linCons
  logM "depGraph" $ show depGraph

  normDepGraph <- toNormalizedDependencyGraph depGraph
  logM "normDepGraph" $ show normDepGraph

  solDepGraph <- toSolvedDependencyGraph normDepGraph
  logM "solDepGraph" $ show solDepGraph

  sol <- toSolution solDepGraph
  pure sol

  -- throwM "escape"

toSolution :: SolvedDependencyGraph -> M Solution
toSolution sdg = 
  if isValidSolution then 
    pure . Solution $ (\(_, _, solDep) -> solDep) <$> Graph.edges sdg
  else 
    throwM $ "the SolvedDependencyGraph is not a valid solution: " ++ show sdg
  where 
    isValidSolution =
      -- each variable is determined by up to one other variable
      all 
        (\x -> length (filter (\(y, _, _) -> x == y) (Graph.edgesTo x sdg)) <= 1)
        (Graph.vertices sdg)

toLinearConstraint :: Constraint Condition Arithmetic Q -> M (Constraint EqualityCondition LinearArithmetic Q)
toLinearConstraint (Cond cond) = do
  let Equal a1 a2 = toEqualityCondition cond
  a1' <- toLinearArithmetic a1
  a2' <- toLinearArithmetic a2
  pure $ Cond (Equal a1' a2')
toLinearConstraint (Prime s) = pure $ Prime s
toLinearConstraint (Fin s) = pure $ Fin s
