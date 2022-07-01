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
import qualified RawConstraint as RC
import Constraints

solve :: Mat -> Constraints -> M (Maybe Constraints)
solve mat cons = do 
  debug 1 $ "mat =\n" ++ displayMat mat ++ "\n"
  -- gaussian elimination
  mat <- gaussElim mat
  debug 1 $ "gaussElim mat =\n" ++ displayMat mat ++ "\n"
  -- eliminate denomenators
  mat <- elimDenoms mat
  debug 1 $ "elimDenoms mat =\n" ++ displayMat mat ++ "\n"
  -- extract constraints
  mb_cons <- extractConstraints mat cons
  -- debug 1 $ "extractConstraints mat =\n" ++ displayConstraints cons ++ "\n"
  -- 
  pure mb_cons

extractConstraints :: Mat -> Constraints -> M (Maybe Constraints)
extractConstraints mat@(Mat rows) cons = do
  -- debug 0 $ "============"
  -- debug 0 $ "rows =\n" ++ unlines (displayRow <$> rows)
  foldM (flip fold) (Just cons) rows
  where 
    fold :: Row -> Maybe Constraints -> M (Maybe Constraints) 
    fold row Nothing = pure Nothing
    fold row (Just cons) = do
      debug 1 $ "============"
      debug 1 $ "row = " ++ displayRow row
      mb_cons <-
        fmap (foldr1 (<|>)) $
        sequence
          [ extractLeq row <*|> cons
          , extractEqu row <*|> cons
          ]
      pure mb_cons
    
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
      case List.findIndex (1 ==) xs of 
        Just j -> do
          let
            f j' _ | j' == j   = 0
            f j' x | otherwise = -x
            equ = Equ j (Expr (mapWithIndex f xs) c)
          pure $ Just \cons -> cons { equs = equ : equs cons } 
        Nothing -> pure Nothing

    extractLeq :: Row -> M (Maybe (Constraints -> Constraints))
    extractLeq (Row xs c) = do
      -- exactly 2 pos coefs, pos const
      let posVars = List.findIndices (0 <) xs
      debug 1 $ "posVars = " ++ show posVars

      case posVars of
        [j1, j2] ->
          if xs!!j1 == 1 then do
            let col_j2 = getCol j2 mat
            if length (filter (0 /=) col_j2) == 1 then do
              -- eliminate j2 since its not used in any other rows
              let leq = Leq j1 
                    (Expr 
                      (
                        (\j x -> 
                           if j `elem` [j1, j2]
                             then 0
                             else -x
                        ) 
                      `mapWithIndex` xs
                      ) 
                    c)
              pure $ Just \cons -> cons 
                { leqs = leq : leqs cons
                , elimVars = j2 : elimVars cons }
            else
              pure Nothing -- j2 cannot be eliminated
          else
            pure Nothing
        _ -> pure Nothing

solveRawConstraints :: Int -> [RC.RawConstraint] -> M (Maybe Constraints)
solveRawConstraints nVars rcs = do
  (mat, cons) <- pure $ foldr (.) id (fromRawConstraint <$> rcs) (Mat [], defaultConstraints nVars)
  mb_cons <- solve mat cons
  pure mb_cons
  where 
    fromRawConstraint :: RC.RawConstraint -> (Mat, Constraints) -> (Mat, Constraints)
    fromRawConstraint (RC.Relation RC.Equ (RC.Exp xs1 c1) (RC.Exp xs2 c2)) (mat, cons) =
      (addRow (toRational <$> Row (zipWith (-) xs1 xs2) (c2 - c1)) mat, cons)
    fromRawConstraint (RC.Relation RC.Leq (RC.Exp xs1 c1) (RC.Exp xs2 c2)) (mat, cons) =
      ( addRow (toRational <$> Row (zipWith (-) xs1 xs2 <> [1]) (c2 - c1)) .
        addEmptyCol $
        mat
      , cons { nVars = nVars + 1 } )
    fromRawConstraint (RC.Relation RC.Geq (RC.Exp xs1 c1) (RC.Exp xs2 c2)) (mat, cons) =
      ( addRow (toRational <$> Row (zipWith (-) xs1 xs2 <> [-1]) (c2 - c1)) .
        addEmptyCol $
        mat
      , cons { nVars = nVars + 1 } )
    fromRawConstraint (RC.Typeclass RC.Prime j) (mat, cons) =
      ( mat
      , cons { primes = j : primes cons, fins = j : fins cons } )
    fromRawConstraint (RC.Typeclass tc j) (mat, cons) =
      ( mat
      , cons { fins = j : fins cons } )