{-# LANGUAGE DeriveFunctor #-}

module Constraints where

import Control.Monad
import Control.Applicative
import qualified Data.List as List
import M
import Q
import Mat
import Inf
import Utils

data Constraints = Constraints 
  { equs :: [Equ]
  , leqs :: [Leq]
  , fins :: [Int]
  , primes :: [Int]
  , elimVars :: [Int] -- eliminated variables
  , nVars :: Int
  }
  deriving (Show)

data Equ = Equ Int (Expr Q) deriving (Show) -- v = a1*x1 + ... + aN*xN + c
data Leq = Leq Int (Expr Q) deriving (Show) -- xj <= a1*x1 + ... + aN*xN + c
data Geq = Geq Int Q deriving (Show) -- c <= xj

data Expr a = Expr [a] a deriving (Show, Functor) -- a1*x1 + ... + aN*xN + c

-- display

displayConstraints cons =
  unlines
    [ "constraints:"
    , "  equs     = " ++ indent (foldMap ("\n" ++) . fmap displayEqu $ equs cons)
    , "  leqs     = " ++ indent (foldMap ("\n" ++) . fmap displayLeq $ leqs cons)
    , "  fins     = " ++ show (fins cons)
    , "  primes   = " ++ show (primes cons)
    , "  elimVars = " ++ show (elimVars cons)
    , "  nVars    = " ++ show (nVars cons)
    ]
  where 
    indent = List.intercalate "\n" . fmap ("    " ++) . lines

displayEqu (Equ j e) = unwords ["(", displayVar j, "=", displayExprQ e, ")"]
displayLeq (Leq j c) = unwords ["(", displayVar j, "<=", displayExprQ c, ")"]
displayGeq (Geq j c) = unwords ["(", displayVar j, ">=", displayQ c, ")"]
displayExprQ (Expr xs c) = unwords [displayRowVars xs, "+", displayQ c]
displayExprInfQ (Expr xs c) = unwords [List.intercalate " + " ((\(x, j) -> displayInfQ x ++ displayVar j)  <$> zip xs [0..]), "+", displayInfQ c]
displayExprInfInt (Expr xs c) = unwords [List.intercalate " + " ((\(x, j) -> displayInfInt x ++ displayVar j)  <$> zip xs [0..]), "+", displayInfInt c]

-- utilities

defaultConstraints :: Int -> Constraints
defaultConstraints nVars = Constraints
  { equs = mempty
  , leqs = mempty
  , fins = mempty 
  , primes = mempty 
  , elimVars = mempty
  , nVars = nVars
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

-- Expr utils

evalExpr :: Num a => Expr a -> [a] -> a
evalExpr (Expr xs c) vs = sum (zipWith (*) xs vs) + c

traverseExpr :: (Traversable t, Applicative t) => (a -> t b) -> Expr a -> t (Expr b)
traverseExpr k (Expr xs c) = Expr <$> traverse k xs <*> k c

toConstant :: (Eq a, Num a) => Expr a -> Maybe a
toConstant (Expr xs c) = do
  guard (all (0 ==) xs)
  pure c

fromConstant :: Num a => Int -> a -> Expr a
fromConstant n c = Expr (replicate n 0) c

-- OLD
-- assertGoodFormed :: Constraints -> M ()
-- assertGoodFormed cons = do
--   assert 
--     ("Constraints are good-formed")
--     (all isTautoRow $ rowsMat (mat cons))
--   where 
--     isTautoRow (Row xs c) = all (0 ==) (c : xs)

equToRow :: Equ -> Row 
equToRow = undefined

leqToRow :: Leq -> Row
leqToRow = undefined

geqToRow :: (Int, Q) -> Row
geqToRow = undefined

