{-# LANGUAGE BlockArguments #-}
module M where

import Control.Monad.Trans
import Control.Monad
import qualified Data.List as List
import qualified ListT as ListT
import Utils
import qualified Test.QuickCheck.Monadic as QCM

-- OLD: type M a = ListT.ListT IO a
type M a = IO a

runM :: M a -> IO a
-- OLD: runM = ListT.toList
runM = id

assert :: String -> Bool -> M ()
assert msg cnd = do
  debugM 1 $ "assert: " ++ msg
  when (not cnd) do
    debugM 0 $ "failed assertion: " ++ msg
    fail "assertion failed"

choose :: Show a => String -> [a] -> M a
choose msg xs = do 
  debugM 1 $ "choose: " ++ msg
  debugM 1 $ "choices: " ++ show xs
  -- OLD: x <- ListT.fromFoldable xs
  if null xs then
    fail "no choices"
  else do
    let x = xs!!0
    debugM 1 $ "==> chosen: " ++ show x
    pure x

chooseST :: Show a => String -> (a -> Bool) -> [a] -> M a
chooseST msg cnd xs = do
  debugM 1 $ "choose: " ++ msg
  xs <- pure $ filter cnd xs
  debugM 1 $ "choices: " ++ show xs
  -- OLD: xs <- pure $ filter cnd xs
  if null xs then
    fail "no choices"
  else do
    let x = xs!!0
    debugM 1 $ "==> chosen: " ++ show x
    pure x

debugLevel = 1 :: Int

debugIO :: Int -> String -> IO ()
debugIO l msg = when (l <= debugLevel) (putStrLn msg)

debugM :: Int -> String -> M ()
-- OLD: debugM l msg = lift (debugIO l msg)
debugM l msg = debugIO l msg