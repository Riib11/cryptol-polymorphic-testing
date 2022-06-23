{-# LANGUAGE BlockArguments #-}

module Solve where

import Control.Monad
import Control.Applicative
import qualified Data.List as List
import Utils
import M
import Q
import Mat
import GaussElim
import Constraints

solve :: Mat -> M Constraints 
solve mat = do 
  debug 1 $ "mat =\n" ++ displayMat mat ++ "\n"
  -- gaussian elimination
  mat <- gaussElim mat
  debug 1 $ "gaussElim mat =\n" ++ displayMat mat ++ "\n"
  -- eliminate denomenators
  mat <- elimDenoms mat
  debug 1 $ "elimDenoms mat =\n" ++ displayMat mat ++ "\n"
  -- -- extract constraints
  cons <- extractConstraints mat
  -- debug 1 $ "extractConstraints mat =\n" ++ displayConstraints cons ++ "\n"
  -- 
  pure cons

extractConstraints :: Mat -> M Constraints
extractConstraints mat@(Mat rows) = do
  -- debug 0 $ "============"
  -- debug 0 $ "rows =\n" ++ unlines (displayRow <$> rows)
  foldM (flip fold) (defaultConstraints (nCols mat)) rows
  where 
    fold :: Row -> Constraints -> M Constraints 
    fold row cons = do
      -- debug 0 $ "============"
      -- debug 0 $ "row = " ++ displayRow row
      mb_cons <-
        fmap (foldr1 (<|>)) $
        sequence
          [ -- extractLeq row >>|= \leq -> pure cons { leqs = leq : leqs cons }
            extractLeq row <*|> cons
          -- , extractEqu row >>|= \equ -> pure cons { equs = equ : equs cons }
          -- TODO: extractCone
          -- TODO: extractCocone
          ]
      case mb_cons of
        Just cons -> pure cons
        Nothing -> error $ "Unable to extract constraint from row: " ++ displayRow row
    
    (<*|>) :: M (Maybe (a -> b)) -> a -> M (Maybe b)
    mf <*|> a = do
      mb_f <- mf
      case mb_f of 
        Just f -> pure . Just $ f a
        Nothing -> pure Nothing

    (>>|=) :: M (Maybe a) -> (a -> M b) -> M (Maybe b)
    m >>|= k = do
      mb_a <- m
      case mb_a of
        Just a -> Just <$> k a
        Nothing -> pure Nothing

    extractEqu :: Row -> M (Maybe (Constraints -> Constraints))
    extractEqu row@(Row xs c) = do
      -- exactly one unit coef, all other neg coefs, pos const
      case List.findIndex (1 ==) xs of
        Just j -> do
          -- debug 0 $ "all (\\(j', x) -> x <= 0 || j == j') (zip [0..] xs) = " ++ show (all (\(j', x) -> x <= 0 || j == j') (zip [0..] xs))
          -- debug 0 $ "0 <= c = " ++ show (0 <= c) 
          if all (\(j', x) -> x <= 0 || j == j') (zip [0..] xs) && 0 <= c then do
            let
              f :: (Q, Int) -> Q
              f (_, j') | j' == j   = 0
              f (x, j') | otherwise = -x
              equ = Equ j (Expr (f <$> zip xs [0..]) c)
            pure $ Just \cons -> cons { equs = equ : equs cons }
          else
            pure Nothing
        Nothing -> pure Nothing

    extractLeq :: Row -> M (Maybe (Constraints -> Constraints))
    extractLeq (Row xs c) = do
      -- exactly 2 pos coefs, pos const
      let posVars = List.findIndices (0 <) xs
      if all (0 <=) xs && length posVars == 2 && 0 <= c then
        case posVars of
          [j1, j2] -> 
            -- the first positif value is 
            -- this row has the only nonzero value in column j2
            if xs!!j1 == 1 && length (filter (0 /=) $ getCol j2 mat) == 1 then do
              let leq = Leq j1 c
              pure $ Just \cons -> cons { leqs = leq : leqs cons }
            else
              pure Nothing
          _ -> pure Nothing
      else 
        pure Nothing

    extractCone :: Row -> M (Maybe Cone)
    extractCone = undefined -- TODO

    extractCocone :: Row -> M (Maybe Cocone)
    extractCocone = undefined -- TODO
