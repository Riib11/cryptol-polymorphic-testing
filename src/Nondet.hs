{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving #-}

module Nondet where

import Control.Applicative

newtype Nondet a = N { unN :: [a] } deriving (Functor, Foldable)

instance Show a => Show (Nondet a) where
  show n = "nondeterministic results:\n" ++ s
    where 
      s = unlines . fmap ((" • " ++) . show) $ unN n

choices :: [a] -> Nondet a
choices = N

instance Alternative Nondet where 
  empty = N mempty
  N as1 <|> N as2 = N (as1 <> as2)

instance Applicative Nondet where
  pure = N . pure
  N fs <*> N as = N [ f a | f <- fs, a <- as ]

instance Monad Nondet where 
  N as >>= k = j $ N (k <$> as)
    where 
      j :: Nondet (Nondet a) -> Nondet a
      j (N ns) = N $ mconcat (unN <$> ns)

prune :: Nondet (Maybe a) -> Nondet a
prune = foldr (\ma -> (maybe empty pure ma <|>)) empty
