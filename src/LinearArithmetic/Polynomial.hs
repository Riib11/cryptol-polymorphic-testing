{-# LANGUAGE LambdaCase, ScopedTypeVariables, TupleSections #-}

module LinearArithmetic.Polynomial where

import ListUtil
import Data.Char as Char
import qualified Data.List as List
import Data.String
import Symbol
import qualified Data.Map as Map
import qualified Data.Set as Set

-- | Polynomial

data Polynomial a = Polynomial a (Map.Map Symbol a) -- a + sum(bi * xi)
  deriving (Eq)

instance (Num a, Eq a, Show a) => Show (Polynomial a) where
  show (Polynomial a m) =
    List.intercalate " + " . mconcat $ 
      [ (\(x, a) -> (if a == 1 then "" else show a) ++ show x) <$> Map.toList m
      , if a == 0 then [] else [show a]
      ]

instance Num a => Semigroup (Polynomial a) where 
  Polynomial a1 m1 <> Polynomial a2 m2 =
    Polynomial (a1 + a2) (Map.unionWith (+) m1 m2)

instance Num a => Monoid (Polynomial a) where 
  mempty = Polynomial 0 mempty

instance Functor Polynomial where 
  fmap f (Polynomial a m) = Polynomial (f a) (f <$> m)

instance Num a => Symbolic (Polynomial a) where 
  symbol x = Polynomial 0 (Map.fromList [(x, 1)])
  symbols (Polynomial a m) = Set.fromList $ Map.keys m

instance (Num a, IsString a) => IsString (Polynomial a) where 
  fromString str = 
    let
      strs_ptms = splitBy "+" str :: [String]
      
      ptms :: [(Maybe Symbol, a)]
      ptms = fmap
        (
          (\case 
            [s] -> 
              if startsWith Char.isAlpha s 
                then (Just (fromString s), 1)
                else (Nothing, fromString s)
            [s1, s2] -> 
              if startsWith Char.isAlpha s1
                then (Just (fromString s1), fromString s2)
                else (Just (fromString s2), fromString s1)
            ss -> error $ "Could not parse term: " ++ show ss
          ) .
          fmap (trim (== ' ')) .
          splitBy "*" .
          trim (== ' ')   
        )
        strs_ptms
      
      tms :: [(Symbol, a)]
      tms = mconcat . fmap (\(m_x, a) -> maybe [] (pure . (,a)) m_x) $ ptms
      
      m_c :: Maybe a
      m_c = List.foldr 
        (\(m_x, c) -> maybe (maybe (pure c) (\_ -> Nothing) m_x) pure) 
        Nothing
        ptms
    in
      Polynomial 
        (maybe 0 id m_c)
        (Map.fromList tms)
    
  -- fromString = symbol . fromString

delete :: Symbol -> Polynomial a -> Polynomial a
delete x (Polynomial a m) = Polynomial a (Map.delete x m)

filter :: Num a => (a -> Bool) -> Polynomial a -> Polynomial a
filter cond (Polynomial _ m) = Polynomial 0 (Map.filter cond m)

singleton :: a -> Polynomial a
singleton a = Polynomial a mempty

constant :: Polynomial a -> a
constant (Polynomial a _) = a

coefficient :: Num a => Symbol -> Polynomial a -> a
coefficient x (Polynomial _ m) = 
  case Map.lookup x m of 
    Just a -> a 
    Nothing -> 0

coefficients :: Polynomial a -> [a]
coefficients (Polynomial a m) = a : Map.elems m

size :: Polynomial a -> Int
size (Polynomial _ m) = Map.size m

scale :: Num a => a -> Polynomial a -> Polynomial a 
scale c = ((c *) <$>)

-- substitute x for p in r
substitute :: Num a => Symbol -> Polynomial a -> Polynomial a -> Polynomial a
substitute x p r = 
  let c = coefficient x p in 
    delete x r <> scale c p