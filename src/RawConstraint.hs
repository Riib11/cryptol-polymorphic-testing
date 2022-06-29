{-# LANGUAGE BlockArguments #-}
module RawConstraint where

import Control.Monad
import Utils
import Data.List
import Q
import M
import qualified Inf as Inf

data RawConstraint
  = Relation Rel Exp Exp
  | Typeclass Tc Int
data Rel = Equ | Leq | Geq
data Exp = Exp [Int] Int
data Tc  = Prime | Fin

instance Show RawConstraint where
  show (Relation r e1 e2) = show e1 ++ " " ++ show r ++ " " ++ show e2
  show (Typeclass tc j) = show tc ++ " " ++ showVar j

instance Show Rel where 
  show Equ = "=="
  show Leq = "<="
  show Geq = ">="

instance Show Exp where
  show (Exp xs c) = intercalate " + " ((\j x -> show x ++ showVar j) `mapWithIndex`  xs) ++ " + " ++ show c

instance Show Tc where 
  show Prime = "prime"
  show Fin = "fin"

showVar j = "x" ++ show j

checkAll :: [Inf.InfInt] -> [RawConstraint] -> IO Bool
-- checkAll vs rcs = all (check vs) rcs
checkAll vs rcs = do
  debugIO 0 $ "Checking assignment: " ++ show vs
  foldM fold True rcs
  where 
    fold b rc = do
      let b' = check vs rc
      when (not b') do
        debugIO 0 $ "Assignment failed raw constraint: " ++ show rc
        case rc of
          Relation _ e1 e2 -> do
            debugIO 0 $ show e1 ++ " ==> " ++ show (evalExp vs e1)
            debugIO 0 $ show e2 ++ " ==> " ++ show (evalExp vs e2)
          _ -> pure ()
      pure (b && b')
  -- all (check vs) rcs

check :: [Inf.InfInt] -> RawConstraint -> Bool
check vs (Relation Equ e1 e2) = evalExp vs e1 == evalExp vs e2
check vs (Relation Leq e1 e2) = evalExp vs e1 <= evalExp vs e2
check vs (Relation Geq e1 e2) = evalExp vs e1 >= evalExp vs e2
check vs (Typeclass Fin j) = Inf.isFin (vs!!j)
check vs (Typeclass Prime j) = error "unimplemented"

evalExp :: [Inf.InfInt] -> Exp -> Inf.InfInt
evalExp vs (Exp xs c) = sum (zipWith (*) vs (Inf.Fin <$> xs)) + Inf.Fin c