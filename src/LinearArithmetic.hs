{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LinearArithmetic where

import Constraint
import qualified Data.List as List
import Data.Map as Map
import qualified Data.Set as Set
import Data.String (IsString (..))
import M
import Field
import N
import Normal
import Q
import Symbol
import Z

import System.IO.Unsafe (unsafePerformIO)

-- | LinearArithmetic
data LinearArithmetic dom
  = Sum dom (Terms dom)
  deriving (Eq)

instance Show dom => Show (LinearArithmetic dom) where
  show (Sum c tms) = show c ++ " + " ++ show tms

instance Field dom => Semigroup (LinearArithmetic dom) where
  Sum c1 tms1 <> Sum c2 tms2 = Sum (c1 .+ c2) (tms1 <> tms2)

instance Field dom => Monoid (LinearArithmetic dom) where
  mempty = Sum zero mempty

instance Field dom => Symbolic (LinearArithmetic dom) where 
  symbol s = Sum zero (symbol s)

  symbols (Sum _ tms) = symbols tms

symbolsLinearArithmetic :: LinearArithmetic dom -> Set.Set Symbol
symbolsLinearArithmetic (Sum _ tms) = symbolsTerms tms

instance Functor LinearArithmetic where
  fmap f (Sum c tms) = Sum (f c) (f <$> tms)

subLA :: (Eq dom, Field dom, Num dom, Show dom) => LinearArithmetic dom -> LinearArithmetic dom -> M (LinearArithmetic dom)
subLA a1@(Sum c1 tms1) a2@(Sum c2 tms2) = do 
  logM "subLA" (show [a1, a2])
  let a = Sum (c1 .- c2) (tms1 <> (negate <$> tms2))
  -- sequence_ [ logM "subLA a'" . show $ normalize' n a | n <- [1..10] ]
  pure (normalize a)

-- subLA (Sum c1 tms1) (Sum c2 tms2) = Sum (c1 .- c2) (tms1 <> (negate <$> tms2))

-- | Terms
newtype Terms dom = Terms (Map Symbol dom) deriving (Eq, Foldable)

unTerms :: Terms dom -> Map Symbol dom
unTerms (Terms m) = m

overTerms :: (Map Symbol dom -> Map Symbol dom) -> (Terms dom -> Terms dom)
overTerms f (Terms m) = Terms (f m)

toTermsList :: Terms dom -> [(Symbol, dom)]
toTermsList (Terms m) = Map.toList m

fromTermsList :: [(Symbol, dom)] -> Terms dom
fromTermsList m = Terms $ Map.fromList m

instance Show dom => Show (Terms dom) where
  show (Terms m) = List.intercalate " + " (["("++ show c ++ ")" ++ show s | (s, c) <- Map.toList m])

instance Field dom => Semigroup (Terms dom) where
  Terms m1 <> Terms m2 =
    Terms $
      Map.foldlWithKey
        ( \m1 s c2 ->
            Map.alter
              ( \case
                  Just c1 -> pure (c1 .+ c2)
                  Nothing -> pure c2
              )
              s
              m1
        )
        m1
        m2

instance Field dom => Monoid (Terms dom) where
  mempty = Terms mempty

instance Field dom => Symbolic (Terms dom) where 
  symbol s = Terms (Map.singleton s one)

  symbols (Terms m) = Set.fromList $ Map.keys m

symbolsTerms :: Terms dom -> Set.Set Symbol 
symbolsTerms (Terms m) = Set.fromList $ Map.keys m 

instance Functor Terms where
  fmap f (Terms m) = Terms (f <$> m)

-- | toLinearArithmetic
toLinearArithmetic :: (Ord dom, Field dom, Show dom) => Arithmetic dom -> M (LinearArithmetic dom)
toLinearArithmetic = proj . normalize
  where
    proj (Val (Con c)) = pure $ Sum one mempty
    proj (Val (Var s)) = proj $ Op Mul (Val (Con one)) (Val (Var s))
    proj (Op Mul (Val (Con c)) (Val (Var s))) = pure $ Sum zero (Terms . Map.fromList $ [(s, c)])
    proj (Op Add a1 a2) = (<>) <$> proj a1 <*> proj a2
    proj a = throwM $ "the `Arithmetic dom` cannot be converted to a `LinearArithmetic dom`: " ++ show a

instance (Eq dom, Field dom, Show dom) => Normal (LinearArithmetic dom) where
  -- remove any terms that have coefficient 0
  simplify (Sum c tms) = Sum c <$> simplify tms

instance (Eq dom, Field dom, Show dom) => Normal (Terms dom) where
  -- remove any terms that have coefficient 0
  simplify (Terms m) | m' <- Map.filter (/= zero) m, Map.size m' < Map.size m = pure $ Terms m'
  simplify (Terms m) = Nothing

coefficients :: Terms dom -> [dom]
coefficients (Terms m) = Map.elems m