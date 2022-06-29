{-# LANGUAGE BlockArguments #-}
module M where

import Control.Monad.Trans
import Control.Monad
import qualified Data.List as List
import qualified ListT as ListT
import qualified Control.Monad.Except as Except
import Utils
import qualified Test.QuickCheck.Monadic as QCM

-- OLD: type M a = ListT.ListT IO a
type M a = Except.ExceptT String IO a

runM :: M a -> IO (Either String a)
-- OLD: runM = ListT.toList
runM = Except.runExceptT

assert :: String -> Bool -> M ()
assert msg cnd = do
  debug 1 $ "assert: " ++ msg
  when (not cnd) do
    debug 0 $ "failed assertion: " ++ msg
    throwError "assertion failed"

choose :: Show a => String -> [a] -> M a
choose msg xs = do 
  debug 1 $ "choose: " ++ msg
  debug 1 $ "choices: " ++ show xs
  -- OLD: x <- ListT.fromFoldable xs
  if null xs then
    throwError "no choices"
  else do
    let x = xs!!0
    debug 1 $ "==> chosen: " ++ show x
    pure x

chooseST :: Show a => String -> (a -> Bool) -> [a] -> M a
chooseST msg cnd xs = do
  debug 1 $ "choose: " ++ msg
  xs <- pure $ filter cnd xs
  debug 1 $ "choices: " ++ show xs
  -- OLD: xs <- pure $ filter cnd xs
  if null xs then
    throwError "no choices"
  else do
    let x = xs!!0
    debug 1 $ "==> chosen: " ++ show x
    pure x

-- debugLevel = -1 :: Int
debugLevel = 0 :: Int

debugIO :: Int -> String -> IO ()
debugIO l msg = when (l <= debugLevel) (putStrLn msg)

debug :: Int -> String -> M ()
-- OLD: debug l msg = lift (debugIO l msg)
debug l msg = lift $ debugIO l msg

throwError :: String -> M a 
throwError = Except.throwError