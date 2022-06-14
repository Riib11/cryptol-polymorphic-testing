module Test where

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
import Solver

test :: IO ()
test = do
  -- let m = Mat 
  --           [ [ 2 , 0 , 1 ]
  --           , [ 0 , 1 , 0 ] ]
  let
    -- mat = 
    --   [ [-1,  1, 0, 1, 0, 2 ]
    --   -- , [ 0, -1, 0, 0, 2, 3 ]
    --   , [ 0,  1, 0, 0, 1, 5 ] ]
    -- vars = Var <$> take (nCols mat) [ [c] | c <- ['a'..]]

    cons =
      -- [ SimpleConstraint (Var "x") Eq (Map.fromList [(Var "y", 1)]) 1 ]
      -- [ ComplexConstraint Eq (Map.fromList [(Var "x", 1), (Var "y", -1)]) 0 ]
      -- [ ComplexConstraint Geq (Map.fromList [(Var "x", 1), (Var "y", -1)]) 0 ]
      mkConstraint <$>
        -- [ -- x - y - z = 10
        --   ([("x", 1), ("y", -1), ("z", -1)], Eq, 10),
        --   -- x >= 1
        --   ([("x", 1)], Geq, 1)
        -- ]
        -- [ -- x - y - z = 10
        --   ([("x", 1), ("y", -1), ("z", -1)], Geq, 10),
        --   -- x = 1
        --   ([("x", 1)], Eq, 1)
        -- ]
        -- [ -- x - y - z = 10
        --   ([("x", 1), ("y", -1)], Geq, 0),
        --   -- x = 1
        --   ([("x", 1)], Eq, 1)
        -- ]
        -- [ -- 2x - 3y = 0
        --   ([("x", 2), ("y", -3)], Eq, 0)
        -- ]
        -- [ ([("x", 1), ("y", -1)], Geq, 0) ] -- x - y >= 10
        -- [ ([("x", 1), ("y", -1)], Eq, 0) ] -- x - y = 10
        -- [ ([("x", 1), ("y", -2)], Eq, 0) ] -- x - y = 10
        -- [ ([("x", 2) , ("y", -3)], Eq, 0) ]
        [ ([("x", 1), ("y", -2)], Leq, 0)
        , ([("x", 1), ("z", -3)], Leq, 0)
        , ([("w", 1)], Leq, 10)
        ]


  results <- runM_unique $ solve cons
  (mvb, cons) : _ <- pure results

  -- putStrLn "===[ results ]==================================================="
  -- putStrLn $
  --   concatMap ("\n • " ++) $
  --   showConstraints $
  --   results
  -- putStrLn ""
  -- putStrLn "================================================================="

  putStrLn "===[ result ]======================================================"
  putStrLn ""
  putStrLn $ "constraints : " ++ showConstraints cons
  -- putStrLn $ "free vars   : " ++ show fvs
  putStrLn $ "var bounds  : " ++ showVarBounds mvb
  putStrLn ""
  putStrLn "==================================================================="

  pure ()