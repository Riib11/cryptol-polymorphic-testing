module Sampler where

import Control.Monad.Trans
import Control.Monad
import qualified Data.List as List
import qualified Data.Map as Map
import System.Random
import qualified Test.QuickCheck as QC
import Var
import M
import Q
import Mat
import Constraint
import SolverLib

data SampleConfig = SampleConfig
  { steps :: Int -- number of points sampled per variable
  }

defaultSampleConfig :: SampleConfig
defaultSampleConfig = SampleConfig
  { steps = 2 }

ticksLin :: SampleConfig -> Int -> Int -> [Int]
ticksLin cfg min max =
  [ min + i*((max - min) `div` steps cfg) | i <- [0..steps cfg - 1]  ]

ticksExp :: SampleConfig -> Int -> [Int]
ticksExp cfg min = 
  [ min + i^2 | i <- [0..2] ]

sample :: SampleConfig -> Map.Map Var VarBound -> Map.Map Var VarValue -> M (Map.Map Var (Maybe Int))
sample cfg bnds vals = do
  -- g <- newStdGen -- TODO: do i need this?
  
  -- debug (-1) $ "bnds = " ++ show bnds
  -- debug (-1) $ "vals = " ++ show vals

  -- computes every possible assignment of sampled vars, from bounds
  m <- foldM
    (\m (x, (lo, mb_up)) -> do
      -- debug (-1) $ show (x, (lo, mb_up))
      case mb_up of
        -- finite upper bound 
        Just up -> do
          -- n <- choose [Just (lo + n * steps cfg) | n <- [0..(lo - up) `div` steps cfg]]
          n <- choose $ Just <$> ticksLin cfg lo up
          pure $ Map.insert x n m
        -- infinite upper bound
        Nothing -> do
          -- n <- choose $ Nothing : (Just <$> ticksExp cfg lo)
          n <- choose $ (Just <$> ticksExp cfg lo) -- TODO: temporarily removed infinity
          pure $ Map.insert x n m
    )
    mempty (Map.toList bnds)

  let
    -- evaluates a var value given an assignment of sampled vars
    evalVarValue :: Map.Map Var (Maybe Int) -> VarValue -> Maybe Int
    evalVarValue m (e, c) = 
        fmap ((c +) . sum) $
        sequence $
        fmap (\(x, c) -> (c *) <$> (m Map.! x)) $
        Map.toList e

  m <- foldM
    (\m (x, val) -> do
      debug 1 $ "m   = " ++ show m
      debug 1 $ "val = " ++ show val
      debug 1 $ "x <- evalVarValue m val = " ++ show (evalVarValue m val)
      pure $ Map.insert x (evalVarValue m val) m)
    m (Map.toList vals)

  pure m


showSampling :: Map.Map Var (Maybe Int) -> String
showSampling smpl =
  List.intercalate ", " $
  fmap (\(x, mb_i) -> show x ++ " = " ++ 
    (case mb_i of 
        Just i -> show i
        Nothing -> "inf"
    )) $
  Map.toList smpl