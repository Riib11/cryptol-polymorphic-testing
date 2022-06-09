{-# LANGUAGE BlockArguments #-}

module SolverLib where

import Control.Monad
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe as Maybe
import Var
import M
import Q
import Mat
import Constraint

-- cons => (consSimple, consComplex)
partitionConstraints :: [Constraint] -> ([Constraint], [Constraint])
partitionConstraints = List.partition isSimpleConstraint

fromComplexConstraintsToComplexEqualities :: [Constraint] -> [(Expr, Q)]
fromComplexConstraintsToComplexEqualities = fmap f
  where 
    f (Relation Eq e q) = (e, q)
    f (Relation Leq e q) = (e `addExpr` freshVarExpr (), q)
    f (Relation Geq e q) = (e `addExpr` negateExpr (freshVarExpr ()), q)

fromComplexEqualitytoRow :: [Var] -> (Expr, Q) -> Row
fromComplexEqualitytoRow vars (e, q) = [ e Map.! v | v <- vars ] <> [q]

-- outputs a list `vs` that maps indices to variables, and a matrix where column `i` of the matrix corresponds to variable `vs!!i`
fromComplexEqualitiesToMatrix :: [(Expr, Q)] -> M ([Var], Mat)
fromComplexEqualitiesToMatrix eqs = do
  let vars = List.nub . concat . fmap (\(e, _) -> Map.keys e) $ eqs
  let rows = fromComplexEqualitytoRow vars <$> eqs 
  let mat = rows
  pure (vars, mat)

-- 1 0 0 0 1
-- 0 0 1 0 1
-- 0 0 0 1 1
gaussElim :: Mat -> M Mat
-- gaussElim m = foldM f m [0..min ((nCols m - 1) - 1) (nRows m - 1)] -- don't do very last col
gaussElim m = go m 0
  where 
    go :: Mat -> Int -> M Mat
    go m j | j > min ((nCols m - 1) - 1) (nRows m - 1) = pure m
    go m@rows j | otherwise = do
      debug 1 $ "m = "
      debug 1 $ showMat m
      debug 3 $ "j = " ++ show j

      -- the first j entries are 0
      let numLeading0s row = length $ takeWhile (0 ==) row
      let minNumLeading0s = 
            foldr1 min $
            filter (>= j) $
            fmap numLeading0s rows

      -- if the entire row of vars are zeros, then entire row must be zeros
      -- otherwise there is no soluton because 0x + 0y = 1 cannot be solved
      debug 1 $ "minNumLeading0s = " ++ show minNumLeading0s
      assert
        ("for each row, minNumLeading0s < nCols m - 1 || last row == 0")
        (all (\row -> minNumLeading0s < nCols m - 1 || last row == 0) rows)

      -- debug 3 $ unlines . fmap unwords $
      --   [ ["mat =", "\n" ++ showMat m]
      --   , ["numLeading0s@rows =", show (numLeading0s <$> rows)]
      --   , ["minNumLeading0s =", show minNumLeading0s]
      --   ]

      -- choose a row such that...
      i <- choose [0..nRows m - 1]
      let row = getRow i m

      -- in this row, the number of leading 0s must be the min among rows that have a number of leading 0s greater than or equal ot j
      assert
        ("numLeading0s row == minNumLeading0s")
        (numLeading0s row == minNumLeading0s)
      -- note that this shadows the previous value of j
      j <- pure minNumLeading0s

      debug 3 $ "j = " ++ show j
      debug 3 $ "i  = " ++ show i

      debug 1 $ "m!(i="++show i++",j="++show j ++") = " ++ showQ (row!!j)
      
      -- get jth entry
      let a = row!!j
      -- divide row by a
      let row' = (/a) <$> row

      debug 1 $ "row' = " ++ showRow row'

      let 
        -- if jth elem is 0, then pass
        -- if jth elem is non-0, then divide by jth elem then subtract row'
        f :: [Q] -> M [Q]
        f r = 
          let b = r!!j in
            if b == 0 
              then pure r
              else do                
                -- scale `row'` by `b` in order to subtract away entire coefficient for focussed variable
                row' <- pure $ (b*) <$> row'
                -- subtract away row'' from r to eliminate `j` variable coefficient
                debug 3 $ "subtracting " ++ showRow r ++ " by " ++ showRow row'
                r <- pure $ r `subList` row'
                debug 3 $ "result: " ++ showRow r
                debug 3 ""
                -- pure $ (`subList` row') . scaleList (1/b) $ r
                pure r
      -- move row to bottom
      -- for each other row:
      --   if jth elem is 0, then pass
      --   if jth elem is non-0, then divide by jth elem then subtract row'
      m <- do
        rows <- pure m
        rows <- pure $ deleteAtList i rows -- delete row'
        debug 1 $ "normalizing rest of rows"
        debug 1 $ "  row' = " ++ showRow row'
        debug 1 $ "  rows = " ++ show (showRow <$> rows)
        rows <- traverse f rows -- eliminate jth col from other rows
        rows <- pure $ rows <> [row'] -- append row'
        pure rows
      
      -- recurse
      go m (j + 1)

normEchelon :: Mat -> M Mat
normEchelon mat@rows = do
  -- for each index (i,i), if m!(i,i) is negative then negate that row
  debug 1 $ "positive-izing rows"
  debug 1 $ "mat = "
  debug 1 $ showMat mat

  rows <-
    mapM
      (\row -> do
        let a = head . dropWhile (0 ==) $ row
        if a < 0
          then pure $ negate <$> row
          else pure row 
      )
      rows
  pure rows

-- takes mapping `v : Int -> Var` and matrix
-- outputs list of equations, where each equation corresponds to one row of the 
-- outputs a list of equations, where each equation corresponds to a row of the matrix, and column `i` of the matrix corresponds to variable `vs!!i`
fromNormEchelonMatrixToEquations :: [Var] -> Mat -> M [Equation]
fromNormEchelonMatrixToEquations vars mat@rows = do
  -- equations, but with ints instead of vars
  eqs <-
    mapM
      (\row -> do
        -- TODO have to modify this to handle the case when one of the column's variables gets totally eliminated
        -- row is in the form [0, ..., 0, 1, a1, ..., aN, c]
        -- corresponds to the equation xi = -a1 x{i+1} + ... + -aNx{n} + c
        let c = last row
        -- index of first non-0 elem
        let i = fromJust $ List.findIndex (0 /=) row 
        -- [0, ..., 0, 0, a1, ..., aN]
        let row' = 
              zipWith
                (\j a -> if i < j && j < length row - 1 then a else 0)
                [0..] row
        let e = Map.fromList $ zip vars row'

        -- -- row augmented with variable indices
        -- -- excludes last element since thats the constant
        -- let rowAug = zip [0..length row - 2] row
        -- -- get just the variables not being solved for
        -- let asAug = init . drop (i + 1) $ rowAug
        -- undefined -- Equation (vars!!i) ()

        pure $ Equation (vars!!i) e c
      )
      rows 
  pure eqs

