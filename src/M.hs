module M where

import Control.Monad.Trans
import Control.Monad
import qualified ListT as ListT

type M a = ListT.ListT IO a

debugLevel :: Int
debugLevel = 0

debug :: Int -> String -> M ()
debug l msg = 
  when (l <= debugLevel)
    (lift . putStrLn $ msg)

runM :: M a -> IO [a]
runM = ListT.toList

choose :: Show a => [a] -> M a 
choose xs = do
  debug 0 $ "choosing from: " ++ show xs
  ListT.fromFoldable xs

assert :: String -> Bool -> M ()
assert msg b =
  if b 
    then pure ()
    else do
      debug 0 $ "rejected; false assertion: " ++ msg
      guard False
