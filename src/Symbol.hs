module Symbol where

import Data.IORef as IORef
import Data.String (IsString(..))
import qualified Data.Set as Set
import System.IO.Unsafe (unsafePerformIO)

-- | Symbol

data Symbol 
  = Symbol String
  | FreshSymbol Int
  deriving (Eq, Ord)

instance IsString Symbol where 
  fromString = Symbol

alphabet = ['a'..'z']

instance Show Symbol where 
  show (Symbol s) = s
  -- show (FreshSymbol i) = "$" ++ [ alphabet !! (i `mod` length alphabet) ]
  show (FreshSymbol i) = "'" ++ [ alphabet !! (i `mod` length alphabet) ]

{-# NOINLINE freshSymbolCountRef #-}
freshSymbolCountRef :: IORef Int
freshSymbolCountRef = unsafePerformIO $ newIORef 0

genFreshSymbol :: () -> Symbol
genFreshSymbol _ = unsafePerformIO $ do
  i <- readIORef freshSymbolCountRef
  modifyIORef freshSymbolCountRef (+1) 
  pure $ FreshSymbol i

-- | Symbolic

class Symbolic a where
  {-# MINIMAL fromSymbol, symbolSet #-}

  fromSymbol :: Symbol -> a
  
  symbolSet :: a -> Set.Set Symbol
  
  fresh :: () -> a
  fresh = fromSymbol . genFreshSymbol

instance Symbolic Symbol where 
  fromSymbol = id
  symbolSet = Set.singleton
  fresh = genFreshSymbol