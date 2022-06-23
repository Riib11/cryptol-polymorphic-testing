module Constraints where

import Control.Monad
import Control.Applicative
import qualified Data.List as List
import M
import Q
import Mat
import Inf
import Utils
import GaussElim

data Constraints = Constraints 
  { equs :: [Equ]
  , leqs :: [Leq]
  , cones :: [Cone]
  , cocones :: [Cocone]
  , elimVars :: [Int] -- eliminated variables
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
    , "  equs     = " ++ indent (foldMap ("\n" ++) . fmap displayEqu $ equs cons)
    , "  leqs     = " ++ indent (foldMap ("\n" ++) . fmap displayLeq $ leqs cons)
    , "  cons     = " ++ "TODO"
    , "  cocons   = " ++ "TODO"
    , "  elimVars = " ++ show (elimVars cons)
    , "  nVars    = " ++ show (nVars cons)
    ]
  where 
    indent = List.intercalate "\n" . fmap ("    " ++) . lines

displayEqu (Equ j e) = unwords ["(", displayVar j, "=", displayExprQ e, ")"]
displayLeq (Leq j c) = unwords ["(", displayVar j, "<=", displayQ c, ")"]
displayGeq (Geq j c) = unwords ["(", displayVar j, ">=", displayQ c, ")"]
displayCone (Cone xs c) = unwords ["(", displayRowVars xs, "<=", displayQ c, ")"]
displayCocone (Cocone c xs) = unwords ["(", displayQ c, "<=", displayRowVars xs, ")"]
displayExprQ (Expr xs c) = unwords [displayRowVars xs, "+", displayQ c]

-- utilities

defaultConstraints :: Int -> Constraints
defaultConstraints nVars = Constraints
  { equs = mempty
  , leqs = mempty
  , cones = mempty
  , cocones = mempty 
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

-- evalExpr
evalExpr :: Expr InfInt -> [InfInt] -> InfInt
evalExpr (Expr xs c) vs = sum (zipWith (*) xs vs) + c

traverseExpr :: (Traversable t, Applicative t) => (a -> t b) -> Expr a -> t (Expr b)
traverseExpr k (Expr xs c) = Expr <$> traverse k xs <*> k c

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
