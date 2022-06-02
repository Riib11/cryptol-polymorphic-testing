{-# LANGUAGE OverloadedStrings, BlockArguments #-}

module Main where

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
import Solver

-- main :: IO ()
-- main = do 
--   putStrLn $ "|1|: " ++ show (normalize (1::Q))
--   putStrLn $ "|0| == |1|: " ++ show ((0::Q) == (1::Q))

main :: IO ()
main = do
  let cons = 
        [ Cond (Re Eq "x1" (1 + "x2"))
        , Cond (Re Le "y1" (1 + "y2"))
        ] :: [Constraint Condition Arithmetic Q]
  labeled "constraints" $ show <$> cons

  mb_sol <- unM $ solve cons
  labeled "solution" $ lines (show mb_sol)
  pure ()

labeled :: String -> [String] -> IO ()
labeled label ls = do
  putStrLn ""
  putStrLn $ "#" ++ replicate 79 '-'
  putStrLn $ "# " ++ label
  putStrLn ""
  mapM_ (putStrLn . ("  " ++)) ls
  putStrLn ""