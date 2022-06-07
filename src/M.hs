module M where 

import Control.Monad.Trans
import ListT
import qualified Data.List as List

type M a = ListT IO a

runM :: M a -> IO [a]
runM = toList

choose :: Show a => [a] -> M a 
choose xs = do
  lift . putStrLn $ "[>] choose from: " ++ show xs 
  fromFoldable xs

impossible :: M a 
impossible = fromFoldable []

debug :: String -> M ()
debug msg = 
  case lines msg of 
    [] -> pure ()
    [s] -> lift . putStrLn $ "[*] " ++ s
    ls -> lift . putStrLn $ "[*] " ++ List.intercalate "\n   " ls