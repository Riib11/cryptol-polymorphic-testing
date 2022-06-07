{-# LANGUAGE LambdaCase, MultiWayIf, OverloadedStrings #-}

module Sample where 

import Control.Monad
import GHC.Real
import qualified Data.Set as Set
import qualified Data.Map as Map
import Symbol
import Expr
import Constraint
import M
import Solving

test :: M ()
test = do
  let cons = 
        -- [ Relation Eq "x" ("y" / 2) ]
        [ Relation Geq "x" "y" ]
  cons <- solve cons
  debug $ show cons

  cons <- traverse toIntegralConstraint cons
  debug $ show cons
  
  cons <- traverse expandGeq cons
  debug $ show cons
  
  cons <- eliminateIntermediates cons
  debug $ show cons

  debug $ "RESULT: " ++ show cons
  
  pure ()

-- -- x ~ (a/b)y --> [x ~ a*z, y = z, z = b*w]
-- toIntegralCoefficients :: Constraint -> M [Constraint]
-- toIntegralCoefficients con@(Relation r a1 (Expr c ts)) = do
--   let xxx = 
--         Map.mapWithKey
--           (\x r@(a :% b) ->
--             if isIntegralRational r
--               then ((x, r), [])
--               else 
--                 let [y, z] = [fresh (), fresh ()] :: [Symbol] in
--                   (
--                     (y, fromIntegral a),
--                     [ Relation 

--                     ])
--                   )
--           ts
--   let con'  = Relation r a1 $ Expr c (Map.fromList (fst <$> Map.elems xxx))
--   let cons' = undefined
--   pure (con' : cons')
-- toIntegralCoefficients con = pure [con]

-- if relation is an equality, then require integral constant and coefficients
-- if relation is an inequality, then round up or down appropriately
-- TODO: not sure if rounding up/down is the right solution
toIntegralConstraint :: Constraint -> M Constraint
toIntegralConstraint con@(Relation Eq a1 a2) = do
  guard $ isIntegralExpr a2
  pure con
toIntegralConstraint con@(Relation Leq a1 a2) =
  pure $ Relation Leq a1 (floorExpr a2)
toIntegralConstraint con@(Relation Geq a1 a2) =
  pure $ Relation Geq a1 (ceilExpr a2)

-- substitute intermediates
-- requires form: solved, integral, expandedGeq
eliminateIntermediates :: [Constraint] -> M [Constraint]
eliminateIntermediates cons = do 
  inters <- intermediates cons
  pure $ foldr (\(x, e) -> (sub x e <$>)) cons inters
  where 
    sub :: Symbol -> Expr -> Constraint -> Constraint
    sub x e (Relation y r d) = Relation y r (subExpr x e d)
    sub x e con = con
    -- an independent variable is a variable that appears on the LHS of an equality and the RHS of an equality
    intermediates :: [Constraint] -> M [(Symbol, Expr)]
    intermediates cons = do
      -- appear on LHS of equality
      let xs_l = foldMap symbolExprEqualityLhs cons :: [(Symbol, Expr)]
      -- xs that appear on RHS of equality
      let xs_lr =
            filter
              (\(x, e) -> any ((x `Set.member`) . symbolSetEqualityRhs) cons) 
              xs_l
      pure xs_lr

-- -- remove constraints that don't have any variables
-- pruneClosed :: 

-- final normal forms:

sample :: [Constraint] -> M (IO Assignment)
sample = undefined

type Assignment = Map.Map Symbol Rational