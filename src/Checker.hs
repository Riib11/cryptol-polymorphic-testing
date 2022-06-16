{-# LANGUAGE TupleSections, BlockArguments #-}
module Checker where 

import qualified Test.QuickCheck as QC
import Control.Monad.Trans
import Control.Monad
import qualified Data.List as List
import qualified Data.Map as Map
import GHC.Real
import Var
import M
import Q
import Mat
import Constraint
import SolverLib
import Solver
import Sampler

-- check that the sampled values do in fact satisfy the constraints
checkConstraint :: Map.Map Var (Maybe Int) -> Constraint -> Bool
checkConstraint m (ComplexConstraint  r e q) = evalRe r (evalExpr m e) (Just q)
checkConstraint m (SimpleConstraint x r e q) = evalRe r (evalVar m x) ((q +) <$> evalExpr m e)

evalExpr :: Map.Map Var (Maybe Int) -> Expr -> Maybe Q
evalExpr m e = fmap sum . sequence $ fmap (\(x, q) -> ((q *) . toRational <$> m Map.! x)) $ Map.toList e

evalVar :: Map.Map Var (Maybe Int) -> Var -> Maybe Q 
evalVar m x = toRational <$> m Map.! x

evalRe :: Re -> (Maybe Q -> Maybe Q -> Bool)
evalRe Eq  = (==)
evalRe Leq = (<=)
evalRe Geq = (>=)

-- arbitraryComplexConstraints :: Int -> Int -> Int -> QC.Gen [Constraint]
-- arbitraryComplexConstraints nVarsDef nVarsCon nCons = do
--   varsDef <- QC.oneof [ pure (Var [c]) | c <- [toEnum 0 .. toEnum (nVarsDef - 1)]]
--   varsCon <- QC.oneof [ pure (Var [c]) | c <- [toEnum nVarsDef .. toEnum (nVarsCon - 1)]]

--   -- replicateM n $
--   -- arbitraryComplexConstraint vars
--   undefined

arbitraryComplexConstraints :: Int -> Int -> QC.Gen [Constraint]
arbitraryComplexConstraints nVars nCons = do
  vars <- arbitraryVars nVars 
  cons <- replicateM nCons (arbitraryComplexConstraint vars)
  pure cons

arbitraryComplexConstraint :: [Var] -> QC.Gen Constraint
arbitraryComplexConstraint vars = 
  ComplexConstraint 
    <$> arbitraryRe
    <*> arbitraryExpr vars
    <*> arbitraryQ

arbitraryRe :: QC.Gen Re
arbitraryRe = QC.oneof $ pure <$> [toEnum 0..]

arbitraryExpr :: [Var] -> QC.Gen Expr
arbitraryExpr vars = normExpr <$> do
  vars <- take (max (length vars `div` 2) 1) <$> QC.shuffle vars
  e <- Map.fromList <$> mapM (\x -> (x,) <$> arbitraryQ) vars
  x <- head <$> QC.shuffle vars
  pure $ Map.insert x (-1) e

arbitraryVars :: Int -> QC.Gen [Var]
arbitraryVars n = pure $ Var <$> [[c] | c <- take n ['a'..'z']]

-- TODO: ok to just choose integers?
arbitraryQ :: QC.Gen Q
arbitraryQ = QC.oneof $ pure <$> [1 .. 10]

-- arbitraryQ :: QC.Gen Q
-- arbitraryQ = do
--   n <- QC.oneof $ pure <$> [1..10]
--   d <- QC.oneof $ pure <$> [1..10]
--   pure (n :% d)