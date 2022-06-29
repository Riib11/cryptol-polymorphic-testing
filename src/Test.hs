{-# LANGUAGE MultiWayIf, BlockArguments #-}
module Test where

import Control.Monad
import Control.Monad.Trans
import qualified Data.List as List
import Utils
import M
import Q
import Inf
import Mat
import GaussElim
import Constraints
import Solve
import Sample
import Test.QuickCheck as QC
import Test.QuickCheck.Monadic as QCM

test :: IO ()
test = do
  let
      mat = Mat 
        [ Row [1, 1] 0
        ]
        -- [ Row [2, -1, -1] 4
        -- , Row [0, 1, -1] 2
        -- ]
        -- -- WARN: non-integral solution
        -- [ Row [3, 0, 4] 1
        -- , Row [0, 1, 2] 1
        -- ]
        -- [ Row [3, 0, 4] 1
        -- , Row [0, 7, 2] 1
        -- , Row [1, 0, 2] 1
        -- ]
        -- -- WARN: not solvable
        -- [ Row [3, 0] 1
        -- , Row [0, 7] 1
        -- , Row [1, 2] 0
        -- ]

  runM do
    -- debug 0 $ "mat:\n" ++ displayMat mat ++ "\n"

    cons <- solve mat
    -- debug 0 $ "solved constraints:\n" ++ displayConstraints cons ++ "\n"
    
    smpl <- sampling cons
    -- debug 0 $ "sampling cons:\n" ++ displaySampling smpl ++ "\n"
    
    vals <- pure $ evalSampling smpl
    -- debug 0 $ "evalSampling smpl:\n" ++ unlines (show <$> vals)
  
    pure ()
  
  pure ()

