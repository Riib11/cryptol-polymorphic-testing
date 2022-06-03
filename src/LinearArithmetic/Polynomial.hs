module LinearArithmetic.Polynomial where

import Data.List as List
import Data.String
import Symbol
import Data.Map as Map
import qualified Data.Set as Set

-- | Polynomial

data Polynomial a = Polynomial a (Map Symbol a) -- a + sum(bi * xi)

instance (Num a, Eq a, Show a) => Show (Polynomial a) where
  show (Polynomial a m) =
    intercalate " + " . mconcat $ 
      [ (\(x, a) -> show a ++ show x) <$> Map.toList m
      , if a == 0 then [] else [show a]
      ]

instance Num a => Semigroup (Polynomial a) where 
  Polynomial a1 m1 <> Polynomial a2 m2 = Polynomial (a1 + a2) (unionWith (+) m1 m2)

instance Num a => Monoid (Polynomial a) where 
  mempty = Polynomial 0 mempty

instance Functor Polynomial where 
  fmap f (Polynomial a m) = Polynomial (f a) (f <$> m)

instance Num a => Symbolic (Polynomial a) where 
  symbol x = Polynomial 0 (fromList [(x, 1)])
  symbols (Polynomial a m) = Set.fromList $ keys m

instance Num a => IsString (Polynomial a) where 
  fromString = symbol . fromString

delete :: Symbol -> Polynomial a -> Polynomial a
delete x (Polynomial a m) = Polynomial a (Map.delete x m)

filter :: Num a => (a -> Bool) -> Polynomial a -> Polynomial a
filter cond (Polynomial _ m) = Polynomial 0 (Map.filter cond m)

singleton :: a -> Polynomial a
singleton a = Polynomial a mempty

constant :: Polynomial a -> a
constant (Polynomial a _) = a

coefficient :: Symbol -> Polynomial a -> a
coefficient x (Polynomial _ m) = m ! x

coefficients :: Polynomial a -> [a]
coefficients (Polynomial a m) = a : elems m

size :: Polynomial a -> Int
size (Polynomial _ m) = Map.size m