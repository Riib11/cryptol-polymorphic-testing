{-# LANGUAGE MultiWayIf, BlockArguments #-}
module Test where

import Control.Monad
import qualified Data.List as List
import Utils
import M
import Q
import Inf
import Mat
import GaussElim
import Constraints
import Solve
import Sample
import Test.QuickCheck as QC
import Test.QuickCheck.Monadic as QCM

test :: IO ()
test = do
  let
      mat = Mat 
        [ Row [1, 1] 0
        ]
        -- [ Row [2, -1, -1] 4
        -- , Row [0, 1, -1] 2
        -- ]
        -- -- WARN: non-integral solution
        -- [ Row [3, 0, 4] 1
        -- , Row [0, 1, 2] 1
        -- ]
        -- [ Row [3, 0, 4] 1
        -- , Row [0, 7, 2] 1
        -- , Row [1, 0, 2] 1
        -- ]
        -- -- WARN: not solvable
        -- [ Row [3, 0] 1
        -- , Row [0, 7] 1
        -- , Row [1, 2] 0
        -- ]

  runM do
    debug (-1) $ "mat:\n" ++ displayMat mat ++ "\n"

    cons <- solve mat
    debug (-1) $ "solved constraints:\n" ++ displayConstraints cons ++ "\n"
    
    smpl <- sampling cons
    debug (-1) $ "sampling cons:\n" ++ displaySampling smpl ++ "\n"
    
    vals <- pure $ evalSampling smpl
    debug (-1) $ "evalSampling smpl:\n" ++ unlines (show <$> vals)
  
  pure ()

-- TODO: try this out
checkSolveSample :: Int -> Int -> PropertyM IO ()
checkSolveSample nVars nCons =
  QCM.forAllM (genMat nVars nCons) \mat -> do
    res <- QCM.run $ runM do
      debug (-1) $ "mat:\n" ++ displayMat mat ++ "\n"

      cons <- solve mat
      debug (-1) $ "solved constraints:\n" ++ displayConstraints cons ++ "\n"
      
      smpl <- sampling cons
      debug (-1) $ "sampling cons:\n" ++ displaySampling smpl ++ "\n"
      
      vals <- pure $ evalSampling smpl
      debug (-1) $ "evalSampling smpl:\n" ++ unlines (show <$> vals)
    case res of
      Right sampling -> do
        -- checkMat sample mat
        pure () -- TODO
      Left msg ->
        pure ()

-- generators

genMat :: Int -> Int -> Gen Mat
genMat nVars nCons = do
  rows <- traverse (genRow nVars) [0..nCons - 1]
  -- shuffle and add multiples of rows to each other
  pure $ Mat rows
  where 
    genPosQ :: Gen Q 
    genPosQ = toRational <$> QC.choose (0, 4 :: Int)

    genPosNzQ :: Gen Q 
    genPosNzQ = toRational <$> QC.choose (1, 4 :: Int)

    genNegQ :: Gen Q 
    genNegQ = negate <$> genPosNzQ

    genRow :: Int -> Int -> Gen Row
    genRow nVars i = oneof
      [ do -- equ
          -- pick a var to be LHS i.e. ==1
          j <- QC.choose (0, nVars - 1)
          -- the rest are negative
          xs <- sequence [ if | j' == j -> pure 1
                              | otherwise -> genPosNzQ
                         | j' <- [0..nVars - 1] ]
          c <- genPosQ
          pure $ Row xs c
      , do -- leq
          -- pick a var to be LHS i.e. ==1
          j1 <- QC.choose (0, nVars - 1)
          -- pick a different var to be RHS i.e. positive
          j2 <- QC.choose (0, nVars - 2)
          j2 <- pure $ if j1 == j2 then j2 + 1 else j2
          -- the rest are 0
          xs <- sequence [ if | j' == j1 -> pure 1
                              | j' == j2 -> genPosNzQ
                              | otherwise -> pure 0
                         | j' <- [0..nVars - 1] ]
          c <- genPosQ
          pure $ Row xs c
      , do -- geq
          -- pick a var to be LHS i.e. ==1
          j1 <- QC.choose (0, nVars - 1)
          -- pick a different var to be RHS i.e. negative
          j2 <- QC.choose (0, nVars - 2)
          j2 <- pure $ if j1 == j2 then j2 + 1 else j2
          -- the rest are 0
          xs <- sequence [ if | j' == j1 -> pure 1
                              | j' == j2 -> genPosNzQ
                              | otherwise -> pure 0
                         | j' <- [0..nVars - 1] ]
          c <- genPosQ
          pure $ Row xs c
      ]

-- check      

-- | Checks to see if the variable assignment satisfies the matrix's equations.
checkMat :: [InfInt] -> Mat -> Bool 
checkMat vs mat@(Mat rows) = all checkRow rows
  where 
    vs' = mapInf toRational <$> vs
    checkRow :: Row -> Bool
    -- v1*x1 + ... + vN*xN == c
    checkRow (Row xs c) = sum (zipWith (*) vs' xs') == c'
      where 
        xs' = Fin <$> xs
        c' = Fin c