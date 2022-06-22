module Constraints where

import Control.Monad
import Control.Applicative
import qualified Data.List as List
import M
import Q
import Mat
import IntInf
import Utils
import GaussElim

data Constraints = Constraints 
  { mat :: Mat
  , equs :: [Equ]
  , leqs :: [Leq]
  , geqs :: [Geq]
  , cones :: [Cone]
  , cocones :: [Cocone]
  , nVars :: Int
  }
  deriving (Show)

data Equ = Equ Int (Expr Q) deriving (Show) -- v = a1*x1 + ... + aN*xN + c
data Leq = Leq Int Q deriving (Show) -- xj <= c
data Geq = Geq Int Q deriving (Show) -- c <= xj
data Cone = Cone [Q] Q deriving (Show) -- a1*x1 + ... + aN*xN <= c
data Cocone = Cocone Q [Q] deriving (Show) -- c <= a1*x1 + ... + aN*xN

data Expr a = Expr [a] a deriving (Show) -- a1*x1 + ... + aN*xN + c

-- display

displayConstraints cons =
  List.intercalate "\n"
    [ "constraints:"
    , "  mat  = \n" ++ indent (displayMat (mat cons))
    , "  equs = " ++ indent (foldMap ("\n" ++) . fmap displayEqu $ equs cons)
    , "  leqs = " ++ indent (foldMap ("\n" ++) . fmap displayLeq $ leqs cons)
    , "  geqs = " ++ indent (foldMap ("\n" ++) . fmap displayGeq $ geqs cons)
    ]
  where 
    indent = List.intercalate "\n" . fmap ("    " ++) . lines

displayEqu (Equ j e) = unwords ["(", displayVar j, "=", displayExprQ e, ")"]

displayLeq (Leq j c) = unwords ["(", displayVar j, "<=", displayQ c, ")"]

displayGeq (Geq j c) = unwords ["(", displayVar j, ">=", displayQ c, ")"]

displayCone (Cone xs c) = unwords ["(", displayRowVars xs, "<=", displayQ c, ")"]

displayCocone (Cocone c xs) = unwords ["(", displayQ c, "<=", displayRowVars xs, ")"]

displayExprQ :: Expr Q -> String
displayExprQ (Expr xs c) = unwords [displayRowVars xs, "+", displayQ c]

-- utilities

defaultConstraints :: Constraints
defaultConstraints = Constraints
  { mat = undefined
  , equs = mempty
  , leqs = mempty
  , geqs = mempty 
  , cones = mempty
  , cocones = mempty 
  , nVars = undefined
  }

defnVars :: Constraints -> [Int]
defnVars cons = (\(Equ j _) -> j) <$> equs cons

freeVars :: Constraints -> [Int]
freeVars cons = filter (not . (`elem` dvs)) [0..nVars cons - 1]
  where dvs = defnVars cons

getEqu :: Int -> Constraints -> Maybe Equ
getEqu j cons = 
  foldr (<|>) Nothing $ 
  map (\equ@(Equ j' _) -> guard (j == j') >> Just equ) $
  equs cons

-- evalExpr
evalExpr :: Expr IntInf -> [IntInf] -> IntInf
evalExpr (Expr xs c) vs = sum (zipWith (*) xs vs) + c

traverseExpr :: (Traversable t, Applicative t) => (a -> t b) -> Expr a -> t (Expr b)
traverseExpr k (Expr xs c) = Expr <$> traverse k xs <*> k c

-- map over mat in constraints
overMat :: Monad m => (Mat -> m Mat) -> Constraints -> m Constraints 
overMat k cons = do
  mat <- k (mat cons)
  pure $ cons { mat = mat, nVars = if nCols mat /= 0 then nCols mat else nVars cons }

-- eliminate denominators
elimDenoms :: Mat -> M Mat
elimDenoms mat = foldM (flip fold) mat [0..nCols mat - 1]
  where
    fold :: Int -> Mat -> M Mat
    fold j mat = do
      -- get lcm, n, of the denomenators of entries in column j of the rows
      let col = getCol j mat
      let dens = denomenatorInt <$> col
      let n = foldr lcm 1 dens
      if n == 1 then
        pure mat
      else do
        -- intro new var, y, that satisfies the eq: xj - n*y = 0
        let row = Row [ if j' == j then
                          1
                        else if j' == (nCols mat - 1) + 1 then 
                          toRational (-n)
                        else
                          0
                      | j' <- [0..(nCols mat - 1) + 1] ] 0
        mat <- pure $ addEmptyCol mat
        mat <- pure $ addRow row mat
        -- gaussian eliminate to propogate new eq
        gaussElim mat -- WARN: may cause infinite loop if variables are solved in a way that keeps producing non-integral coefficients. but i think that inserting the new equation at the top of the new matrix will prevent this

extractFromMat :: Constraints -> M Constraints
extractFromMat cons = do
  Mat rows <- pure $ mat cons
  (equs' , rows') <- foldM (flip fold) ([], []) rows
  pure $ cons { mat = Mat rows', equs = equs' }
  where 
    fold :: Row -> ([Equ], [Row]) -> M ([Equ], [Row])
    fold row (equs', rows') = do
      case extractEqu row of 
        Just equ -> pure (equ : equs', rows')
        Nothing -> pure (equs', row : rows')

extractEqu :: Row -> Maybe Equ
extractEqu (Row xs c) = do
  j <- List.findIndex (1 ==) xs -- index of first 1
  let
    f :: (Q, Int) -> Q
    f (_, j') | j' == j   = 0
    f (x, j') | otherwise = -x
  pure $ Equ j $ Expr (f <$> zip xs [0..]) c

extractLeq :: Row -> Maybe Leq
extractLeq = undefined -- TODO

extractGeq :: Row -> Maybe Geq
extractGeq = undefined -- TODO

extractCone :: Row -> Maybe Cone
extractCone = undefined -- TODO

extractCocone :: Row -> Maybe Leq
extractCocone = undefined -- TODO

assertGoodFormed :: Constraints -> M ()
assertGoodFormed cons = do
  assert 
    ("Constraints are good-formed")
    (all isTautoRow $ rowsMat (mat cons))
  where 
    isTautoRow (Row xs c) = all (0 ==) (c : xs)