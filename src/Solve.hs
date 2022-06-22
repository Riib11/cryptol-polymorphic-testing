module Solve where

import Control.Monad
import qualified Data.List as List
import Utils
import M
import Q
import Mat
import GaussElim
import Constraints

solve :: Constraints -> M Constraints 
solve cons = do 
  -- gaussian elimination
  cons <- overMat gaussElim cons
  debugM 0 $ "overMat gaussElim cons =\n" ++ displayConstraints cons ++ "\n"
  -- eliminate denomenators
  cons <- overMat elimDenoms cons
  debugM 0 $ "overMat elimDenoms cons =\n" ++ displayConstraints cons ++ "\n"
  -- extract equations
  cons <- extractFromMat cons
  debugM 0 $ "extractFromMat cons =\n" ++ displayConstraints cons ++ "\n"
  -- assert good-formed
  assertGoodFormed cons
  -- 
  pure cons