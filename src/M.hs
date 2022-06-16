module M where

import Control.Monad.Trans
import Control.Monad
import qualified Data.List as List
import qualified ListT as ListT
import Utils
import qualified Test.QuickCheck.Monadic as QCM

type M a = ListT.ListT IO a

debugLevel :: Int
debugLevel = -1

debugQCM :: Int -> String -> QCM.PropertyM IO ()
debugQCM l msg = QCM.run $ debugIO l msg

debugIO :: Int -> String -> IO ()
debugIO l msg = when (l <= debugLevel) (putStrLn $ msg)

debug :: Int -> String -> M ()
debug l msg = 
  when (l <= debugLevel)
    (lift . putStrLn $ msg)

runM :: M a -> IO [a]
runM = ListT.toList

runM_unique :: Eq a => M a -> IO [a]
runM_unique = fmap List.nub . runM

choose :: Show a => [a] -> M a 
choose xs = do
  debug 0 $ "branching on choice from: " ++ show xs
  ListT.fromFoldable xs

assert :: String -> Bool -> M ()
assert msg b =
  if b 
    then pure ()
    else do
      debug 0 $ "branch rejected due to false assertion: " ++ msg ++ "\n"
      guard False

reject :: String -> M a
reject msg = do
  debug 0 $ "branch rejected: " ++ msg
  fail ("branch rejected: " ++ msg)

shuffle :: Show a => [a] -> M [a]
shuffle xs = do
  debug 0 $ "shuffling: " ++ show xs
  go xs
  where
    go [] = pure []
    go xs = do
      i <- ListT.fromFoldable [0..length xs - 1]
      ((xs!!i) :) <$> go (deleteAtList i xs)

shuffleBy :: [Int] -> [a] -> [a]
shuffleBy p xs = go p
  where 
    go [] = []
    go (i:p) = (xs!!i) : go p

toFirst :: M a -> IO a
toFirst m = do
  (a : _) <- ListT.toList m
  pure a