module Test where

import Control.Monad
import qualified Data.List as List
import Utils
import M
import Q
import Mat
import GaussElim
import Constraints
import Solve
import Sample
import Test.QuickCheck

genConstraints :: Int -> Int -> Gen Constraints 
genConstraints nEqus = do



-- test :: IO ()
-- test = do
--   let
--     cons = defaultConstraints
--       { mat = Mat 
--           [ Row [2, -1, -1] 4
--           , Row [0, 1, -1] 2
--           ]
--         -- mat = Mat -- WARN: non-integral solution
--         --   [ Row [3, 0, 4] 1
--         --   , Row [0, 1, 2] 1
--         --   ]
--         -- mat = Mat 
--         --   [ Row [3, 0, 4] 1
--         --   , Row [0, 7, 2] 1
--         --   , Row [1, 0, 2] 1
--         --   ]
--         -- -- not solvable
--         -- mat = Mat 
--         --   [ Row [3, 0] 1
--         --   , Row [0, 7] 1
--         --   , Row [1, 2] 0
--         --   ]
--       , nVars = 3
--       }
  
--   cons <- runM $ solve cons
--   putStrLn $ "solved constraints:\n" ++ displayConstraints cons ++ "\n"
  
--   smpl <- sampling cons
--   putStrLn $ "sampling cons:\n" ++ displaySampling smpl ++ "\n"
  
--   vals <- pure $ evalSampling smpl
--   putStrLn $ "evalSampling smpl:\n" ++ unlines (show <$> vals)
  
--   pure ()