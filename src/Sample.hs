{-# LANGUAGE BlockArguments, LambdaCase, MultiWayIf, TupleSections #-}
module Sample where

import Control.Monad.Trans
import Control.Monad
import qualified ListT
import qualified Control.Monad.State as State
import qualified Control.Monad.Except as Except
import qualified Data.List as List
import Data.Maybe
import Utils
import M
import Q
import Inf
import Mat
import GaussElim
import Constraints
import System.IO.Unsafe (unsafePerformIO)

-- | Sampling

data Sample
  = SampleRange [Expr InfQ] -- x <= min{bounds}
  | SampleSingle (Expr InfQ) -- x = e
  deriving (Show)

type UpperBound = [Expr InfQ]

fin_largest :: (Num a, Ord a, Show a) => Inf a
fin_largest = 128

displaySampling :: [Sample] -> String
displaySampling smpls = List.intercalate "\n" $ f `mapWithIndex` smpls
  where 
    f j (SampleRange es) = displayVar j ++ " <= min{" ++ List.intercalate ", " (displayExprInfQ <$> es) ++ "}"
    f j (SampleSingle e) = displayVar j ++ " = " ++ displayExprInfQ e

sampleConstraints :: Constraints -> M [Sample]
sampleConstraints cons = do
  bounds <- collectUpperBounds cons
  traverse f (zip [0..] bounds)
  where 
    f :: (Int, UpperBound) -> M Sample
    f (j, bound) = 
      if 
        | Just (Equ _ e) <- List.find (\(Equ j' _) -> j == j') (equs cons) -> 
            pure $ SampleSingle (Fin <$> e)
        | otherwise ->
            pure $ SampleRange bound

collectUpperBounds :: Constraints -> M [UpperBound]
collectUpperBounds cons =
  flip (foldM (flip foldEqus)) (equs cons) >=>
  flip (foldM (flip foldLeqs )) (leqs cons) 
  $
  [ if | j `elem` elimVars cons -> defaultUpperBound_elim
       | j `elem` fins cons     -> defaultUpperBound_fin
       | otherwise              -> defaultUpperBound_inf
  | j <- [0..n - 1]
  ]
  where
    n = nVars cons

    -- example:
    --    x1 = a2*x2 - a3*x3 - a4*x4 - a5*x5 + c
    --    ==> x3 <= (a2*x2 + c) / a3
    --    ==> x4 <= ((a2*x2 + c / a3) - x3) / a4
    --    ==> x5 <= ((((a2*x2 + c / a3) - x3) / a4) - x4) / a5
    foldEqus :: Equ -> [UpperBound] -> M [UpperBound]
    foldEqus (Equ j e@(Expr as c)) bounds = do
      debug 2 $ "foldEqus.e = " ++ displayExprQ e
      -- e = a1*x1 + ... aN*xN + c
      let posVars = List.findIndices (> 0) as
      let negVars = List.findIndices (< 0) as
      snd <$> foldM (flip f) (Expr ((\j a -> if j `elem` posVars then a else 0) `mapWithIndex` as) c, bounds) negVars
      where 
        -- j is the index of a negative variable
        f :: Int -> (Expr Q, [UpperBound]) -> M (Expr Q, [UpperBound])
        f j (e, bounds) = do
          debug 2 $ "foldEqus.f.j = " ++ show j
          aj <- pure $ abs (as!!j)
          -- divide RHS by aj
          e@(Expr as c) <- pure $ (/aj) <$> e
          -- xj <= e/aj
          -- since xj is being subtracted, must be finite
          bounds <- pure $ modifyAtList j
            (((Fin <$> e) : defaultUpperBound_fin) <>) 
            bounds
          -- subtract jth var from RHS
          e <- pure $ Expr (modifyAtList j (\_ -> -1) as) c
          pure (e, bounds)

    foldLeqs :: Leq -> [UpperBound] -> M [UpperBound]
    foldLeqs (Leq j e) = pure . modifyAtList j ((Fin <$> e) :)

    defaultUpperBound_elim, defaultUpperBound_fin, defaultUpperBound_inf :: UpperBound
    defaultUpperBound_elim = [fromConstant n 0]
    defaultUpperBound_fin  = [fromConstant n fin_largest]
    defaultUpperBound_inf  = [fromConstant n PosInf]

-- | Evaluation

type Preval = Either (Either [Expr InfQ] (Expr InfQ)) InfQ
type EvalM a = State.StateT [Preval] (ListT.ListT (Except.ExceptT String IO)) a

execEvalM :: EvalM () -> [Preval] -> M [[Preval]]
execEvalM m st = ListT.toList $ State.execStateT m st

toPreval :: Sample -> Preval 
toPreval (SampleSingle e) = Left (Right e)
toPreval (SampleRange es) = Left (Left es)

fromPreval :: Preval -> Maybe InfQ 
fromPreval (Right x) = Just x
fromPreval _ = Nothing

evalSampling :: [Sample] -> M [[InfQ]]
evalSampling sampling = do
  debug 1 $ "sampling = \n" ++ displaySampling sampling
  res <- execEvalM evalVars (toPreval <$> sampling)
  case sequence . fmap sequence . fmap (fmap fromPreval) $ res of
    Just res -> pure res
    Nothing -> throwError "evalSampling: not everything was fully evaluated"
  where
    n = length sampling :: Int

    evalVars :: EvalM ()
    evalVars = mapM_ evalVar [0..n - 1]

    evalVar :: Int -> EvalM InfQ
    evalVar j = do
      preEval <- (!!j) <$> State.get
      case preEval of
        Right x -> do
          lift . lift $ debug 1 $ "eval " ++ displayVar j ++ " => " ++ displayInfQ x
          pure x
        Left (Right e) -> do
          x <- evalExpr e
          setVar j x
          lift . lift $ debug 1 $ "eval " ++ displayVar j ++ " => " ++ displayInfQ x
          pure x
        Left (Left es) -> do
          xs <- evalExpr `traverse` es
          x <- lift $ ListT.fromFoldable $ sampleFixedRange xs 
          setVar j x
          lift . lift $ debug 1 $ "eval " ++ displayVar j ++ " => " ++ displayInfQ x
          pure x

    evalExpr :: Expr InfQ -> EvalM InfQ
    evalExpr e@(Expr xs c) = do 
      vs <- (\(j, x) ->
          if x == 0
            then pure 0
            else evalVar j)
        `traverse` ([0..n - 1] `zip` xs)
      lift . lift $ debug 1 $ "eval " ++ displayExprInfQ e ++ " => " ++ displayInfQ (Constraints.evalExpr e vs)
      pure $ Constraints.evalExpr e vs

    setVar :: Int -> InfQ -> EvalM ()
    setVar j x = State.modify $ modifyAtList j (\_ -> Right x)

    sampleFixedRange :: [InfQ] -> [InfQ]
    sampleFixedRange xs = 
      let x = minimum xs in
        if x == 0
          then [0]
          else [0, x]

mapChoices :: (a -> [b]) -> [a] -> [[b]]
mapChoices k []    = []
mapChoices k [x]   = pure <$> k x
mapChoices k (x:xs)= (:) <$> k x <*> mapChoices k xs

