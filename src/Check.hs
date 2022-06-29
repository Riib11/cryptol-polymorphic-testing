{-# LANGUAGE MultiWayIf, BlockArguments #-}
module Check where

import Control.Monad
import Control.Monad.Trans
import qualified Data.List as List
import Utils
import M
import Q
import Inf
import Mat
import GaussElim
import Constraints
import qualified RawConstraint as RC
import Solve
import Sample
import Test.QuickCheck as QC
import Test.QuickCheck.Monadic as QCM

-- mapM_ putStrLn =<< (fmap (unlines . fmap show) <$> sample' (genRawConstraints 3 2))

check :: IO ()
check = quickCheck (checkSolveSample 3 2)
-- check = quickCheck (checkSolveSample 3 1)
-- check = quickCheck $ once $ monadicIO $
--   checkSolveRawConstraints 3 
--     [RC.Relation RC.Leq (RC.Exp [1, 0, 0] 0) (RC.Exp [0, 0, 2] 2)]
  

-- TODO: try this out
checkSolveSample :: Int -> Int -> Property -- PropertyM IO ()
checkSolveSample nVars nCons = monadicIO $
  -- OLD: forAllM (genMat nVars nCons) \mat -> do
  forAllM (genRawConstraints nVars nCons) (checkSolveRawConstraints nVars)
    

checkSolveRawConstraints :: Int -> [RC.RawConstraint] -> PropertyM IO Bool
checkSolveRawConstraints nVars rcs = do
  run $ debugIO 0 $ "============================================"
  res <- run $ runM do
    debug 0 $ "raw constraints:\n" ++ unlines (show <$> rcs)

    cons <- solveRawConstraints nVars rcs
    debug 0 $ "solved constraints:\n" ++ displayConstraints cons
    
    smpl <- sampling cons
    debug 0 $ "sampling cons:\n" ++ displaySampling smpl ++ "\n"
    
    vals <- pure $ evalSampling smpl
    debug 0 $ "evalSampling smpl:\n" ++ unlines (show <$> vals)

    pure vals
  case res of
    Right asgns -> do
      tests <- run $ mapM
        (\asgn -> RC.checkAll asgn rcs)
        asgns
      pure $ and tests
    Left msg -> do
      run $ debugIO 0 $ "throwError: " ++ msg
      pure False

-- generators

genRawConstraints :: Int -> Int -> Gen [RC.RawConstraint]
genRawConstraints nVars nCons = do
  rcs <- concat <$> traverse go [0..nCons - 1]
  pure rcs
  where
    genInt :: Gen Int
    genInt = QC.choose (-4, 4)

    genInt_positive :: Gen Int
    genInt_positive = QC.choose (0, 4)

    go :: Int -> Gen [RC.RawConstraint]
    go i | i < nVars = do
      xs1 <- sequence
        [ if | j == i -> pure 1
             | j /= i -> pure 0
        | j <- [0..nVars - 1] ]
      c1 <- pure 0
      lhs <- pure $ RC.Exp xs1 c1
      
      xs2 <- sequence
        [ if | j <= i -> pure 0
             | j >  i  -> genInt_positive
        | j <- [0..nVars - 1] ]
      c2 <- pure $ abs $ foldr lcm 1 (filter (0 /=) xs2)
      rhs <- pure $ RC.Exp xs2 c2
      
      fins <- pure $ RC.Typeclass RC.Fin <$> List.findIndices (< 0) xs2
      
      r <- oneof $ pure <$> [RC.Equ, RC.Leq, RC.Geq]
      -- r <- oneof $ pure <$> [RC.Equ, {- FIX RC.Leq -} RC.Geq]
      -- r <- oneof $ pure <$> [RC.Leq]
      
      pure [ RC.Relation r lhs rhs ]


-- OLD
-- genMat :: Int -> Int -> Gen Mat
-- genMat nVars nCons = do
--   rows <- traverse genRow [0..nCons - 1]
--   -- shuffle and add multiples of rows to each other
--   pure $ Mat rows
--   where 
--     genInt_positive :: Gen Int
--     genInt_positive = QC.choose (0, 4)

--     genInt_positiveNz :: Gen Int 
--     genInt_positiveNz = QC.choose (1, 4)

--     genNeg :: Gen Int
--     genNeg = negate <$> genInt_positiveNz

--     genRow :: Int -> Gen Row
--     genRow i | i < nVars = do
--       xs <- sequence 
--         [ if | j <  i               -> pure 0
--              | j == i               -> pure 1
--              | i <  j && j <  nCons -> pure 0
--              | i <  j && j >= nCons -> genNeg
--         | j <- [0..nVars - 1]
--         ]
--       c <- pure $ abs $ foldr (*) 1 (filter (0 /=) xs)
--       pure $ Row (toRational <$> xs) (toRational c)
--     genRow i | otherwise = do
--       -- TODO: when more constraints than vars??
--       pure $ Row [ 0 | _ <- [0..nVars - 1] ] 0

      -- [ do -- equ
      --     -- pick a var to be LHS i.e. ==1
      --     j <- QC.choose (0, nVars - 1)
      --     -- the rest are negative
      --     xs <- sequence [ if | j' == j -> pure 1
      --                         | otherwise -> genInt_positiveNz
      --                    | j' <- [0..nVars - 1] ]
      --     c <- pure $ foldr lcm 1 (filter (0 /=) xs)
      --     pure $ Row (toRational <$> xs) (toRational c)
      -- , do -- leq
      --     -- pick a var to be LHS i.e. ==1
      --     j1 <- QC.choose (0, nVars - 1)
      --     -- pick a different var to be RHS i.e. positive
      --     j2 <- QC.choose (0, nVars - 2)
      --     j2 <- pure $ if j1 == j2 then j2 + 1 else j2
      --     -- the rest are 0
      --     xs <- sequence [ if | j' == j1 -> pure 1
      --                         | j' == j2 -> genInt_positiveNz
      --                         | otherwise -> pure 0
      --                    | j' <- [0..nVars - 1] ]
      --     c <- pure $ foldr lcm 1 (filter (0 /=) xs)
      --     pure $ Row (toRational <$> xs) (toRational c)
      -- , do -- geq
      --     -- pick a var to be LHS i.e. ==1
      --     j1 <- QC.choose (0, nVars - 1)
      --     -- pick a different var to be RHS i.e. negative
      --     j2 <- QC.choose (0, nVars - 2)
      --     j2 <- pure $ if j1 == j2 then j2 + 1 else j2
      --     -- the rest are 0
      --     xs <- sequence [ if | j' == j1 -> pure 1
      --                         | j' == j2 -> genInt_positiveNz
      --                         | otherwise -> pure 0
      --                    | j' <- [0..nVars - 1] ]
      --     c <- pure $ foldr lcm 1 (filter (0 /=) xs)
      --     pure $ Row (toRational <$> xs) (toRational c)
      -- ]

-- check      

-- OLD
-- -- | Checks to see if the variable assignment satisfies the matrix's equations.
-- checkMat :: [InfInt] -> Mat -> Bool 
-- checkMat vs mat@(Mat rows) = all checkRow rows
--   where 
--     vs' = mapInf toRational <$> vs
--     checkRow :: Row -> Bool
--     -- v1*x1 + ... + vN*xN == c
--     checkRow (Row xs c) = sum (zipWith (*) vs' xs') == c'
--       where 
--         xs' = Fin <$> xs
--         c' = Fin c

-- OLD
-- displayCheckMat :: [InfInt] -> Mat -> IO ()
-- displayCheckMat vs mat@(Mat rows) = mapM_ displayCheckRow rows
--   where
--     vs' = mapInf toRational <$> vs
--     displayCheckRow :: Row -> IO ()
--     -- v1*x1 + ... + vN*xN == c
--     displayCheckRow (Row xs c) =
--       putStrLn $ "checking equality: " ++
--         (List.intercalate " + " . fmap displayInfQ $ zipWith (*) vs' xs') ++ " = " ++ displayInfQ c'
--       where 
--         xs' = Fin <$> xs
--         c' = Fin c