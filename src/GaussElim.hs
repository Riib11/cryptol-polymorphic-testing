module GaussElim where

import Control.Monad
import qualified Data.List as List
import M
import Q
import Mat
import Utils

-- | Performs gaussian elimination on a matrix.
gaussElim :: Mat -> M Mat
gaussElim mat = do
  go mat 0
  where
    -- jth column
    go :: Mat -> Int -> M Mat 
    go mat j | j > min (nCols mat - 1) (nRows mat - 1) = do
      assert 
        ("The matrix must be solvable.")
        (isSolvable mat)
      debugM 1 "done with gaussElim"
      pure mat
    go mat j | otherwise = do
      debugM 1 $ "(nCols mat - 1) = " ++ show (nCols mat - 1)
      debugM 1 $ "(nRows mat - 1) = " ++ show (nRows mat - 1)
      debugM 1 $ "j = " ++ show j

      assert 
        ("The matrix must be solvable.")
        (isSolvable mat)

      let minCountLeading0s = 
            foldr1 min $
            filter (j <=) $
            countLeading0sRows mat

      debugM 1 $ "minCountLeading0s = " ++ show minCountLeading0s
      if j == minCountLeading0s then do
        i <- chooseST
              ("Choosing row as representative for column." )
              (\i -> countLeading0sRow (getRow i mat) == minCountLeading0s)
              [0 .. nRows mat - 1]

        mat <- simplifyRow i j mat
        mat <- eliminateCol i j mat
        go mat (j + 1)
      else do
        debugM 1 $ "skipping column " ++ show j
        go mat (j + 1)

-- | Simplifies row i at column j by dividing row i by it's column j entry
simplifyRow :: Int -> Int -> Mat -> M Mat
simplifyRow i j mat = do
  let x = getEntry i j mat 
  assert
    ("Simplify row " ++ show i ++ " at column " ++ show j ++ " by dividing by nonzero entry m[i,j] = " ++ displayQ x)
    (x /= 0)
  pure $ scaleRowMat i (1/x) mat

-- | Eliminates column j from each row, other than row i, by subtracting a multiple of row i equal to the column j entry of the row
eliminateCol :: Int -> Int -> Mat -> M Mat
eliminateCol i j mat = foldM (flip fold) mat (deleteAtList i [0..nRows mat - 1])
  where
    row = getRow i mat
    fold i' = eliminateColRow i' j row

-- | Eliminates column j from row i by subtracting a multiple of the given row equal to the column j entry of the row
eliminateColRow :: Int -> Int -> Row -> Mat -> M Mat
eliminateColRow i j row mat = do
  let x = getEntry i j mat
  pure $ subRowMat i ((x *) <$> row) mat
