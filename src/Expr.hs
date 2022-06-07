module Expr where 

import Data.String
import GHC.Real
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Symbol

data Expr = Expr Rational Terms 
  deriving (Eq)

instance Show Expr where 
  show (Expr c ts) = 
    List.intercalate " + " $ 
      ((\(x, a) -> showRational a ++ "*" ++ show x) <$> Map.toList ts) <> 
      [showRational c]
    
instance IsString Expr where 
  fromString = fromSymbol . fromString

instance Num Expr where
  (+) = addExpr
  
  negate = mapExpr negate 
  
  e1 * e2 | isConstant e1 = (constant e1 *) `mapExpr` e2
  e1 * e2 | isConstant e2 = (constant e2 *) `mapExpr` e1
  e1 * e2 | otherwise = error $ "Can only multiply an Expr by a constant Expr, but attempted to compute \"" ++ show e1 ++ " * " ++ show e2 ++ "\""

  abs = mapExpr abs

  signum = error "Cannot signum an Expr"

  fromInteger = fromConstant . fromIntegral

instance Fractional Expr where 
  fromRational = fromConstant 
  e1 / e2 | isConstant e2 = (/ constant e2) `mapExpr` e1
  e1 / e2 | otherwise = error $ "Can only divide an Expr by a constant Expr, but attempted to compute \"" ++ show e1 ++ " / " ++ show e2 ++ "\""

showRational :: Rational -> String
showRational (a :% b) | b == 1 && a >= 0 = show a
showRational (a :% b) | b == 1 && a <  0 = "(" ++ show a ++ ")"
showRational (a :% b) | b /= 1 && a >= 0 = "(" ++ show a ++ "/" ++ show b ++ ")"

type Terms = Map.Map Symbol Rational

isSingleton :: Expr -> Bool
isSingleton (Expr c m) = c == 0 && Map.size m == 1

isConstant :: Expr -> Bool
isConstant (Expr c m) = Map.null m

instance Symbolic Expr where 
  fromSymbol x = Expr 0 (Map.fromList [(x, 1)])
  symbolSet (Expr _ ts) = Set.fromList $ Map.keys ts

normalizeExpr :: Expr -> Expr 
normalizeExpr (Expr a ts) = Expr a (Map.filter (/= 0) ts)

addExpr :: Expr -> Expr -> Expr 
addExpr (Expr a1 ts1) (Expr a2 ts2) = normalizeExpr $
  Expr (a1 + a2) (Map.unionWith (+) ts1 ts2)

mapExpr :: (Rational -> Rational) -> (Expr -> Expr)
mapExpr f (Expr a ts) = normalizeExpr $ Expr (f a) (f <$> ts)

fromConstant :: Rational -> Expr 
fromConstant a = Expr a mempty

coefficients :: Expr -> [Rational]
coefficients (Expr _ m) = Map.elems m

coefficient :: Symbol -> Expr -> Rational
coefficient x (Expr _ ts) = 
  case Map.lookup x ts of
    Just r -> r 
    Nothing -> 0

constant :: Expr -> Rational
constant (Expr c _) = c

terms :: Expr -> Map.Map Symbol Rational
terms (Expr _ m) = m

deleteSymbol :: Symbol -> Expr -> Expr 
deleteSymbol x (Expr c m) = Expr c (Map.delete x m)

subExpr :: Symbol -> Expr -> Expr -> Expr 
subExpr x e' e@(Expr c ts) = 
  deleteSymbol x e `addExpr` ((* coefficient x e) `mapExpr` e')

ceilExpr :: Expr -> Expr 
ceilExpr = mapExpr 
  (\x -> if x < 0 then fromIntegral (floor x) else fromIntegral (ceiling x))

floorExpr :: Expr -> Expr 
floorExpr = mapExpr 
  (\x -> if x < 0 then fromIntegral (ceiling x) else fromIntegral (floor x))

isIntegralExpr :: Expr -> Bool
isIntegralExpr (Expr c m) = isIntegralRational c && all isIntegralRational m
  
isIntegralRational :: Rational -> Bool  
isIntegralRational x = x == fromIntegral (ceiling x)

numerator, denomenator :: Rational -> Integer
numerator (_ :% b) = b
denomenator (a :% _) = a