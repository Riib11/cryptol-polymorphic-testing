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

solve :: Mat -> Constraints -> M Constraints 
solve mat cons = do 
  debug 1 $ "mat =\n" ++ displayMat mat ++ "\n"
  -- gaussian elimination
  mat <- gaussElim mat
  debug 1 $ "gaussElim mat =\n" ++ displayMat mat ++ "\n"
  -- eliminate denomenators
  mat <- elimDenoms mat
  debug 1 $ "elimDenoms mat =\n" ++ displayMat mat ++ "\n"
  -- -- extract constraints
  cons <- extractConstraints mat cons
  -- debug 1 $ "extractConstraints mat =\n" ++ displayConstraints cons ++ "\n"
  -- 
  pure cons

extractConstraints :: Mat -> Constraints -> M Constraints
extractConstraints mat@(Mat rows) cons = do
  -- debug 0 $ "============"
  -- debug 0 $ "rows =\n" ++ unlines (displayRow <$> rows)
  foldM (flip fold) cons rows
  where 
    fold :: Row -> Constraints -> M Constraints 
    fold row cons = do
      debug 0 $ "============"
      debug 0 $ "row = " ++ displayRow row
      mb_cons <-
        fmap (foldr1 (<|>)) $
        sequence
          [ 
            extractLeq row <*|> cons
          , extractEqu row <*|> cons
          --   extractLeq row >>|= \leq -> pure cons { leqs = leq : leqs cons }
          -- , extractEqu row >>|= \equ -> pure cons { equs = equ : equs cons }
          -- TODO: extractCone
          -- TODO: extractCocone
          ]
      case mb_cons of
        Just cons -> pure cons
        Nothing -> throwError $ "Unable to extract constraint from row:\n  " ++ displayRow row ++ "\n\n" ++ show row
    
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
          debug 0 $ "all (\\(j', x) -> x <= 0 || j == j') (zip [0..] xs) = " ++ show (all (\(j', x) -> x <= 0 || j == j') (zip [0..] xs))
          debug 0 $ "0 <= c = " ++ show (0 <= c) 
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
      debug 0 $ "posVars = " ++ show posVars

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

      -- if length posVars == 2 && length negVars <= 1 && 0 <= c then do
      --   case posVars of
      --     [j1, j2] -> 
      --       -- the first positif value is 
      --       -- this row has the only nonzero value in column j2
      --       if xs!!j1 == 1 then 
      --         case negVars of 
      --           [] -> do
      --             let leq = Leq j1 c
      --             pure $ Just \cons -> cons { leqs = leq : leqs cons }
      --           [j3] ->
      --             -- TODO: make sure that j3 is not used in any other row
      --             -- TODO eliminate j3 and turn into a Leq
      --       else
      --         pure Nothing
      --     _ -> pure Nothing
      -- else 
      --   pure Nothing

    extractCone :: Row -> M (Maybe Cone)
    extractCone = undefined -- TODO

    extractCocone :: Row -> M (Maybe Cocone)
    extractCocone = undefined -- TODO

solveRawConstraints :: Int -> [RC.RawConstraint] -> M Constraints
solveRawConstraints nVars rcs = do
  let (mat, cons) = foldr (.) id (fromRawConstraint <$> rcs) (Mat [], defaultConstraints nVars)
  cons <- solve mat cons
  pure cons
  where 
    fromRawConstraint :: RC.RawConstraint -> (Mat, Constraints) -> (Mat, Constraints)
    fromRawConstraint (RC.Relation RC.Equ (RC.Exp xs1 c1) (RC.Exp xs2 c2)) (mat, cons) =
      (addRow (toRational <$> Row (zipWith (-) xs1 xs2) (c2 - c1)) mat, cons)
    fromRawConstraint (RC.Relation RC.Leq (RC.Exp xs1 c1) (RC.Exp xs2 c2)) (mat, cons) =
      ( addRow (toRational <$> Row (zipWith (-) xs1 xs2 <> [1]) (c2 - c1)) .
        addEmptyCol $
        mat
      , cons)
    fromRawConstraint (RC.Relation RC.Geq (RC.Exp xs1 c1) (RC.Exp xs2 c2)) (mat, cons) =
      ( addRow (toRational <$> Row (zipWith (-) xs1 xs2 <> [-1]) (c2 - c1)) .
        addEmptyCol $
        mat
      , cons)
    fromRawConstraint (RC.Typeclass RC.Prime j) (mat, cons) =
      ( mat
      , cons { primes = j : primes cons } )
    fromRawConstraint (RC.Typeclass tc j) (mat, cons) =
      ( mat
      , cons { fins = j : fins cons } )