{-# LANGUAGE BlockArguments #-}

module Solver where

import Control.Monad.Trans
import Control.Monad
import Data.List as List
import Data.Map as Map
import Var
import M
import Q
import Mat
import Constraint
import SolverLib

-- tries to reduce a set of contraints into a set of simple constraints
solve :: [Constraint] -> M [Constraint]
solve cons = do
  -- partition contraints into simple and complex
  let (consSimple, consComplex) = partitionConstraints cons
  -- if 0 complex constraints, then done
  if length consComplex == 0 then
    pure consSimple
  else do
    -- convert complex constraints into complex equalities
    let eqsComplex = fromComplexConstraintsToComplexEqualities consComplex
    -- normalize complex equalities
    let eqsComplex' = fmap (\(e, q) -> (normExpr e, q)) eqsComplex
    -- convert complex equalities into matrix
    (vs, mat) <- fromComplexEqualitiesToMatrix eqsComplex
    -- gaussElim matrix
    mat <- gaussElim mat
    -- normEchelon matrix 
    mat <- normEchelon mat
    -- convert matrix into equations
    eqs <- fromNormEchelonMatrixToEquations vs mat
    -- substitute equations in simple constraint
    let cons' = subConstraint eqs <$> consSimple
    -- recurse on resulting set of constraints
    solve cons'

test :: IO ()
test = do
  -- let m = Mat 
  --           [ [ 2 , 0 , 1 ]
  --           , [ 0 , 1 , 0 ] ]
  let m = [ [-1,  1, 0, 1, 0, 2 ]
          , [ 0, -1, 0, 0, 2, 3 ]
          , [ 0,  1, 0, 0, 1, 5 ] ]
  -- let m = [ [1, 0, 0, 1]
  --         , [2, 0, 0, 1] ]
  ms <- runM $ do
    -- lift . putStrLn $ "m =\n" ++ showMat m
    m <- gaussElim m
    -- lift . putStrLn $ "gaussElim m =\n" ++ showMat m
    m <- normEchelon m
    -- lift . putStrLn $ "normEcheon m =\n" ++ showMat m
    pure m
  ms <- pure $ List.nub ms -- remove duplicate solutions
  putStrLn "===[ solutions ]==================================================="
  mapM ((putStrLn "" >>) . putStrLn . showMat) ms
  putStrLn ""
  pure ()