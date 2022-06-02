module Symbol where

import Data.IORef as IORef
import Data.String (IsString(..))
import Data.Set
import System.IO.Unsafe (unsafePerformIO)

-- | Symbol

data Symbol 
  = Symbol String
  | FreshSymbol Int
  deriving (Eq, Ord)

instance IsString Symbol where 
  fromString = Symbol

instance Show Symbol where 
  show (Symbol s) = s
  show (FreshSymbol i) = "@" ++ show i

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
  {-# MINIMAL symbol, symbols #-}

  symbol :: Symbol -> a
  
  symbols :: a -> Set Symbol
  
  freshSymbol :: () -> a
  freshSymbol = symbol . genFreshSymbol
