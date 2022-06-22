{-# LANGUAGE BlockArguments, LambdaCase #-}
module Sample where

import Control.Monad
import qualified Data.List as List
import Utils
import M
import Q
import IntInf
import Mat
import GaussElim
import Constraints
import System.IO.Unsafe (unsafePerformIO)

-- Var => IntInf
data Sample
  = SampleRange [IntInf]
  | SampleExpr (Expr IntInf)
  deriving (Show)

data Bound = Bound Int IntInf deriving (Show)

displaySampling :: [Sample] -> String
displaySampling sampling = 
  "sampling:\n" ++
  ( List.intercalate "\n" $
    fmap ("  " ++) $
    mapWithIndex (\j sample -> unwords [displayVar j, displaySample sample]) sampling)

displaySample :: Sample -> String 
displaySample (SampleRange vs) = "in " ++ show vs
displaySample (SampleExpr e  ) = ":= " ++ displayExprIntInf e

displayBound :: Bound -> String
displayBound (Bound lo hi) = "[" ++ show lo ++ ", " ++ show hi ++ "]"

displayExprIntInf :: Expr IntInf -> String
displayExprIntInf (Expr xs c) = unwords [displayRowVarsIntInf xs, "+", show c]
  where 
    displayRowVarsIntInf = 
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
    bnds = collectBounds cons
    fvs = freeVars cons
    f j bnd =
      if j `elem` fvs then
        SampleRange $ sampleVar bnd
      else
        case getEqu j cons of 
          Just (Equ _ e) -> 
            case traverseExpr (fmap Fin . fromRationalToInt) e of 
              Just e' -> SampleExpr e'
              Nothing -> error $ "The equation for " ++ displayVar j ++ " is non-integral."
          Nothing -> error $ "The equation for " ++ displayVar j ++ " was not found."

-- utilities

defaultBound = Bound 0 PosInf

sampleVar :: Bound -> [IntInf]
sampleVar (Bound lo hi) | Fin lo > hi = []
sampleVar (Bound lo (Fin hi)) = if lo == hi then [Fin lo] else [Fin lo, Fin hi]
sampleVar (Bound lo PosInf) = [Fin lo, PosInf]

collectBounds :: Constraints -> [Bound]
collectBounds cons = 
  flip (foldr foldLeqs ) (leqs cons) .
  flip (foldr foldGeqs ) (geqs cons) $
  flip (foldr foldCones) (cones cons) $
  [ defaultBound | _ <- [0..nVars cons - 1] ]
  where 
    foldLeqs  (Leq j c)   = modifyAtList j (\(Bound lo hi) -> Bound (lo `max` ceiling c) hi)
    foldGeqs  (Geq j c)   = modifyAtList j (\(Bound lo hi) -> Bound lo (hi `min` fromInteger (floor c)))
    foldCones (Cone xs c) = foldr (.) id $
      (\j x ->
        if x /= 0 then
          modifyAtList j \(Bound lo hi) -> Bound lo (hi `min` fromInteger (floor c))
        else
          id :: [Bound] -> [Bound]
      ) `mapWithIndex` xs

evalSampling :: [Sample] -> [[IntInf]]
evalSampling sampling = do
  -- return $! unsafePerformIO $ putStrLn $ "evalSamplingFree " ++ show sampling ++ " = " ++ show (evalSamplingFree sampling)
  samplingFree <- evalSamplingFree sampling
  res <- evalSamplingDefn samplingFree samplingFree
  -- return $! unsafePerformIO $ putStrLn $ "evalSamplingDefn " ++ show samplingFree ++ " = " ++ show (evalSamplingDefn samplingFree samplingFree)
  -- return $! unsafePerformIO $ putStrLn $ "==="
  pure res
  where
    evalSamplingFree :: [Sample] -> [[Either (Expr IntInf) IntInf]]
    evalSamplingFree = mapChoices \case
        SampleRange vs -> Right <$> vs
        SampleExpr  e  -> [Left e]

    evalSamplingDefn :: [Either (Expr IntInf) IntInf] -> [Either (Expr IntInf) IntInf] -> [[IntInf]]
    evalSamplingDefn samplingFree = mapChoices \case 
      Right v -> [v]
      Left  e -> [evalSampleExpr samplingFree e]

    evalSampleExpr :: [Either (Expr IntInf) IntInf] -> Expr IntInf -> IntInf
    evalSampleExpr samplingFree (Expr xs c) = sum (f `mapWithIndex` (zip xs samplingFree)) + c
      where 
        f j (0, _      ) = Fin 0 -- WARN: should always have 0 coefficient when j is same index as the var this equation defines
        f j (x, Right v) = x * v
        f j (x, Left e ) = x * evalSampleExpr samplingFree e

mapChoices :: (a -> [b]) -> [a] -> [[b]]
mapChoices k []    = []
mapChoices k [x]   = pure <$> k x
mapChoices k (x:xs)= (:) <$> k x <*> mapChoices k xs
