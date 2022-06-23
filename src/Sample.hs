{-# LANGUAGE BlockArguments, LambdaCase #-}
module Sample where

import Control.Monad
import qualified Data.List as List
import Utils
import M
import Q
import Inf
import Mat
import GaussElim
import Constraints
import System.IO.Unsafe (unsafePerformIO)

-- TODO: actually, don't need to worry worry about lower bounds, since they just turn into x = y + c where x is lower-bounded by c

-- Var => InfInt
data Sample
  = SampleRange [InfInt]
  | SampleExpr (Expr InfInt)
  deriving (Show)

data UpperBound 
  = UpperBound InfInt deriving (Show)

displaySampling :: [Sample] -> String
displaySampling sampling = 
  "sampling:\n" ++
  ( List.intercalate "\n" $
    fmap ("  " ++) $
    mapWithIndex (\j sample -> unwords [displayVar j, displaySample sample]) sampling)

displaySample :: Sample -> String 
displaySample (SampleRange vs) = "in " ++ show vs
displaySample (SampleExpr e  ) = ":= " ++ displayExprInfInt e

displayUpperBound :: UpperBound -> String
displayUpperBound (UpperBound up) = "<= " ++ show up

displayExprInfInt :: Expr InfInt -> String
displayExprInfInt (Expr xs c) = unwords [displayRowVarsInfInt xs, "+", show c]
  where 
    displayRowVarsInfInt = 
      List.intercalate " + " .
      mapWithIndex (\j x -> show x ++ displayVar j)

-- sampling

-- samples from solved constraints
-- • sample the free variables
-- • compute the other variables based on sampling results
sampling :: Constraints -> M [Sample]
sampling cons = do
  pure $ f `mapWithIndex` bnds
  where
    bnds = collectUpperBounds cons
    zeroExpr = Expr [0 | _ <- [0..nVars cons - 1]] 0
    f j bnd =
      if j `elem` elimVars cons then
        SampleExpr zeroExpr -- just set eliminated vars to 0
      else if j `elem` freeVars cons then
        SampleRange $ sampleVar bnd
      else
        case getEqu j cons of 
          Just (Equ _ e) -> 
            case traverseExpr (fmap Fin . fromRationalToInt) e of 
              Just e' -> SampleExpr e'
              Nothing -> error $ "The equation for " ++ displayVar j ++ " is non-integral."
          Nothing -> error $ "The equation for " ++ displayVar j ++ " was not found."

-- utilities

defaultUpperBound = UpperBound PosInf

sampleVar :: UpperBound -> [InfInt]
sampleVar (UpperBound (Fin 0)) = [0]
sampleVar (UpperBound (Fin up)) = [0, Fin up]
sampleVar (UpperBound PosInf) = [0, PosInf]

collectUpperBounds :: Constraints -> [UpperBound]
collectUpperBounds cons = 
  flip (foldr foldLeqs ) (leqs cons) .
  flip (foldr foldCones) (cones cons) $
  [ if j `elem` elimVars cons 
      then UpperBound (Fin 0)
      else defaultUpperBound 
  | j <- [0..nVars cons - 1] ]
  where 
    foldLeqs  (Leq j c)   = modifyAtList j (\(UpperBound up) -> UpperBound (up `min` fromInteger (ceiling c)))
    foldCones (Cone xs c) = foldr (.) id $
      (\j x ->
        if x /= 0 then
          modifyAtList j \(UpperBound up) -> UpperBound (up `min` fromInteger (floor c))
        else
          id :: [UpperBound] -> [UpperBound]
      ) `mapWithIndex` xs

evalSampling :: [Sample] -> [[InfInt]]
evalSampling sampling = do
  -- return $! unsafePerformIO $ putStrLn $ "evalSamplingFree " ++ show sampling ++ " = " ++ show (evalSamplingFree sampling)
  samplingFree <- evalSamplingFree sampling
  res <- evalSamplingDefn samplingFree samplingFree
  -- return $! unsafePerformIO $ putStrLn $ "evalSamplingDefn " ++ show samplingFree ++ " = " ++ show (evalSamplingDefn samplingFree samplingFree)
  -- return $! unsafePerformIO $ putStrLn $ "==="
  pure res
  where
    evalSamplingFree :: [Sample] -> [[Either (Expr InfInt) InfInt]]
    evalSamplingFree = mapChoices \case
        SampleRange vs -> Right <$> vs
        SampleExpr  e  -> [Left e]

    evalSamplingDefn :: [Either (Expr InfInt) InfInt] -> [Either (Expr InfInt) InfInt] -> [[InfInt]]
    evalSamplingDefn samplingFree = mapChoices \case 
      Right v -> [v]
      Left  e -> [evalSampleExpr samplingFree e]

    evalSampleExpr :: [Either (Expr InfInt) InfInt] -> Expr InfInt -> InfInt
    evalSampleExpr samplingFree (Expr xs c) = sum (f `mapWithIndex` (zip xs samplingFree)) + c
      where 
        f j (0, _      ) = Fin 0 -- WARN: should always have 0 coefficient when j is same index as the var tups equation defines
        f j (x, Right v) = x * v
        f j (x, Left e ) = x * evalSampleExpr samplingFree e

mapChoices :: (a -> [b]) -> [a] -> [[b]]
mapChoices k []    = []
mapChoices k [x]   = pure <$> k x
mapChoices k (x:xs)= (:) <$> k x <*> mapChoices k xs
