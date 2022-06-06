module Debug where 

import System.IO.Unsafe (unsafePerformIO)

debugM :: Monad m => String -> m ()
debugM msg = return $! unsafePerformIO $ putStrLn msg

debug :: String -> a -> a 
debug msg a = unsafePerformIO $ do
  debugM msg 
  pure a