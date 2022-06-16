module Var where 

import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

data Var = Var String | FreshVar Int deriving (Eq, Ord)

primeVar :: Var -> Var 
primeVar (Var s) = Var (s ++ "'")
primeVar (FreshVar _) = freshVar ()

instance Show Var where 
  show (Var s) = s
  show (FreshVar i) = "'" ++ [alphabet !! (i `mod` length alphabet)]
    where 
      alphabet = ['a'..'z']

{-# NOINLINE freshVarCounter #-}
freshVarCounter :: IORef Int
freshVarCounter = unsafePerformIO $ newIORef 0

freshVar :: () -> Var 
freshVar _ = unsafePerformIO $ do
  v <- FreshVar <$> readIORef freshVarCounter
  modifyIORef freshVarCounter (1+)
  pure v

isFreshVar :: Var -> Bool
isFreshVar (FreshVar _) = True
isFreshVar _ = False
