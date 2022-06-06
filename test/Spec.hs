{-# LANGUAGE OverloadedStrings, BlockArguments, BangPatterns #-}

module Main where

import Control.Arrow
import qualified Data.Set as Set
import qualified Data.Map as Map
import ListT
import Q
import LinearArithmetic.Polynomial
import Constraint
import Solver
import Sampler
import Debug

-- main :: IO ()
-- main = do 
--   putStrLn $ "|1|: " ++ show (normalize (1::Q))
--   putStrLn $ "|0| == |1|: " ++ show ((0::Q) == (1::Q))

type PQ = Polynomial Q

main :: IO ()
main = do
  let 
    cons :: [Constraint Q]
    cons = 
      -- [ Relation Eq "x" "y + 1/2" ]
      -- [ Relation Eq "x + 4" "2*y" 
      -- , Relation Eq "z" "w" ]
      -- [ Relation Ge "y" "-2" ]
      [ Relation Eq "x" "y + 1" ]
  labeled "constraints" $ show <$> cons

  sols <- ListT.toList $ solve cons
  labeled "solutions" $ (show <$> sols)

  maps <- ListT.toList $ do
    sol0 <- solve cons
    debugM $ "sol0: " ++ show sol0
    sol1 <- expandGes sol0
    debugM $ "sol1: " ++ show sol1
    sol2 <- substituteIntermediates sol1
    debugM $ "sol2: " ++ show sol2
    map <- toSampleRanges sol2
    debugM $ "map: " ++ show map
    pure map
  labeled "sample range" (show . Map.toList <$> maps)

  pure ()

labeled :: String -> [String] -> IO ()
labeled label ls = do
  putStrLn ""
  putStrLn $ "#" ++ replicate 79 '-'
  putStrLn $ "# " ++ label
  putStrLn ""
  mapM_ (putStrLn . ("  " ++)) ls
  putStrLn ""