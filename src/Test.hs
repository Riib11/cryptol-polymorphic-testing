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
import qualified RawConstraint as RC
import Solve
import Sample
import Test.QuickCheck as QC
import Test.QuickCheck.Monadic as QCM

test :: IO ()
test = do
  let 
      -- n = 4
      -- rcs = 
      --   [ RC.Relation RC.Equ (RC.Exp [1, 0] 0) (RC.Exp [0, 2] 0) -- x = y
      --   , RC.Relation RC.Geq (RC.Exp [0, 1] 0) (RC.Exp [0, 0] 1) -- y >= 1
      --   ]
      --   [ RC.Relation RC.Equ (RC.Exp [1, 0, 0, 0] 0) (RC.Exp [0, 2, -1, -1] 0) ] -- x = y - z - w
      -- n = 3
      -- rcs = [ RC.Relation RC.Equ (RC.Exp [1, 1, 1] 0) (RC.Exp [0, 0, 0] 3) ]
      n = 3
      rcs = [ RC.Relation RC.Leq (RC.Exp [1, 0, 0] 0) (RC.Exp [0, 1, 1] 0) ]
  ex_valss <- runM do
    mb_cons <- solveRawConstraints n rcs
    cons <- case mb_cons of 
      Just cons -> pure cons 
      Nothing -> throwError "solving resulting in Nothing"

    debug 0 $ "================================================================"
    debug 0 $ displayConstraints cons
    
    smpl <- sampleConstraints cons
    valss <- evalSampling smpl
    pure valss
  case ex_valss of
    Left msg -> putStrLn $ "[error] " ++ msg
    Right valss -> do
      putStrLn "[success]"
      mapM_ (\vals -> putStrLn $ unwords $ fmap displayInfQ $ vals) valss
      -- TODO: why isn't trying inf???
  pure ()

-- test :: IO ()
-- test = do
--   let
--       mat = Mat 
--         [ Row [1, 1] 0
--         ]
--         -- [ Row [2, -1, -1] 4
--         -- , Row [0, 1, -1] 2
--         -- ]
--         -- -- WARN: non-integral solution
--         -- [ Row [3, 0, 4] 1
--         -- , Row [0, 1, 2] 1
--         -- ]
--         -- [ Row [3, 0, 4] 1
--         -- , Row [0, 7, 2] 1
--         -- , Row [1, 0, 2] 1
--         -- ]
--         -- -- WARN: not solvable
--         -- [ Row [3, 0] 1
--         -- , Row [0, 7] 1
--         -- , Row [1, 2] 0
--         -- ]

--   runM do
--     -- debug 0 $ "mat:\n" ++ displayMat mat ++ "\n"

--     cons <- solve mat
--     -- debug 0 $ "solved constraints:\n" ++ displayConstraints cons ++ "\n"
    
--     smpl <- sampling cons
--     -- debug 0 $ "sampling cons:\n" ++ displaySampling smpl ++ "\n"
    
--     vals <- pure $ evalSampling smpl
--     -- debug 0 $ "evalSampling smpl:\n" ++ unlines (show <$> vals)
  
--     pure ()
  
--   pure ()

