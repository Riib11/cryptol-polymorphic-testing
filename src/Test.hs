{-# LANGUAGE BlockArguments #-}
module Test where

import Control.Monad.Trans
import Control.Monad
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Monadic as QCM
import GHC.Real
import Var
import M
import Q
import Mat
import Constraint
import SolverLib
import Solver
import Sampler
import Checker

nVars = 4
nCons = 3

bug :: QC.Property -- QCM.PropertyM IO ()
bug = QC.once $ QCM.monadicIO do
  -- BUG: problematic subtracting infinity in original constraint
  -- let consIn = [(ComplexConstraint Geq (Map.fromList [(Var "b", ((2):%(1))), (Var "c", ((-1):%(1)))]) ((6):%(1)) ),(ComplexConstraint Eq (Map.fromList [(Var "a", ((-1):%(1))), (Var "c", ((1):%(1)))]) ((6):%(1)) ),(ComplexConstraint Eq (Map.fromList [(Var "b", ((6):%(1))), (Var "d", ((-1):%(1)))]) ((1):%(1)) )]
  -- BUG: bug in my testing framework, since it cares whether vars are fresh or not, but my printer doesn't do that
  -- let consIn = [(ComplexConstraint Geq (Map.fromList [(Var "b", ((-1):%(1))), (Var "d", ((1):%(1)))]) ((7):%(1)) ),(ComplexConstraint Geq (Map.fromList [(Var "a", ((-1):%(1))), (Var "b", ((1):%(1)))]) ((4):%(1)) ),(ComplexConstraint Eq (Map.fromList [(Var "a", ((10):%(1))), (Var "b", ((-1):%(1)))]) ((4):%(1)) )]
  let consIn = 
        [ ComplexConstraint Eq (Map.fromList [(Var "a", 1), (Var "b", -1/2), (Var "c", -2/3)]) 0 ]

  debugQCM (-1) $ showConstraints consIn

  debugQCM (-1) $ "========================================================="
  res <- QCM.run $ runM_unique $ solve consIn
  debugQCM (-1) $ "consIn  = " ++ showConstraints consIn
  case res of
    [] ->
      debugQCM (-1) $ "res     = " ++ show res
    ((bnds, vals, consOut) : _) -> do
      debugQCM (-1) $ "consOut = " ++ showConstraints consOut
      debugQCM (-1) $ "bnds    = " ++ showVarBounds bnds
      debugQCM (-1) $ "vals    = " ++ showVarValues vals
      debugQCM (-1) $ ""
      smpls <- QCM.run $ runM $ sample defaultSampleConfig bnds vals
      mapM_ (\smpl -> 
        mapM_ (\con -> do
          debugQCM (-1) $ unlines
            [ "checking:"
            , "  smpl = {" ++ showSampling smpl ++ "}"
            , "  con  = {" ++ showConstraint con ++ "}"
            ]
          QCM.assert (checkConstraint smpl con)
          ) consIn
        ) smpls
  pure ()

prop :: QC.Property
prop = QCM.monadicIO do
  debugQCM 0 $ "========================================================="
  consIn <- QCM.pick $ arbitraryComplexConstraints nVars nCons
  res <- QCM.run $ runM_unique $ solve consIn
  debugQCM 0 $ "consIn  = " ++ showConstraints consIn
  case res of
    [] ->
      debugQCM 0 $ "res     = " ++ show res
    ((bnds, vals, consOut) : _) -> do
      debugQCM 0 $ "consOut = " ++ showConstraints consOut
      debugQCM 0 $ "bnds    = " ++ showVarBounds bnds
      debugQCM 0 $ "vals    = " ++ show vals -- showVarValues vals
      smpls <- QCM.run $ runM $ sample defaultSampleConfig bnds vals
      mapM_ (\smpl -> 
        mapM_ (\con -> do
          debugQCM 0 $ unlines
            [ "checking:"
            , "  smpl = {" ++ showSampling smpl ++ "}"
            , "  con  = {" ++ showConstraint con ++ "}"
            ]
          QCM.assert (checkConstraint smpl con)
          ) consIn
        ) smpls

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

    consIn =
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

  results <- runM_unique $ solve consIn
  (bnds, vals, consOut) : _ <- pure results
  
  smpls <- runM $ sample 
    defaultSampleConfig
      bnds
      vals

  putStrLn ""
  putStrLn "===[ result ]======================================================"
  putStrLn ""
  putStrLn $ "input constraints  : " ++ showConstraints consIn
  putStrLn $ "output constraints : " ++ showConstraints consOut
  putStrLn $ "var equalities     : " ++ showVarValues vals
  putStrLn $ "var bounds         : " ++ showVarBounds bnds
  putStrLn ""
  
  putStrLn "===[ sample ]======================================================"
  putStrLn $ concatMap ("\n • " ++) $ showSampling <$> smpls
  putStrLn ""

  putStrLn "===[ checks ]======================================================"
  putStrLn ""

  chks <- mapM
    (\smpl -> do
      -- make sure that smpl satisfies all original constraints
      chks <- mapM
        (\con -> do
          if not (checkConstraint smpl con) then do
            putStrLn $ "FAILURE"
            putStrLn $ "  sampling   : " ++ showSampling smpl
            putStrLn $ "  constraint : " ++ showConstraint con
            pure False
          else
            pure True
        )
        consIn
      pure $ all id chks
    )
    smpls
  let (nPass, nFail) = foldr (\chk (nPass, nFail) -> if chk then (nPass + 1, nFail) else (nPass, nFail + 1)) (0, 0) chks
  putStrLn $ "total checks   : " ++ show (nPass + nFail)
  putStrLn $ "passing checks : " ++ show nPass
  putStrLn $ "failing checks : " ++ show nFail


  putStrLn ""
  putStrLn "==================================================================="