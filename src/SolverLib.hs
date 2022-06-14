{-# LANGUAGE BlockArguments, LambdaCase #-}

module SolverLib where

import Control.Monad
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe as Maybe
import Var
import M
import Q
import Mat
import Constraint
import Utils

-- cons => (consSimple, consComplex)
partitionConstraints :: [Constraint] -> ([Constraint], [Constraint])
partitionConstraints = List.partition isSimpleConstraint

partitionSimpleConstraints :: [Constraint] -> ([Constraint], [Constraint])
partitionSimpleConstraints = List.partition isSimpleEquality

-- can't have x = 1 and x = 2
assertUniqueSimpleConstantEqualities :: [Constraint] -> M ()
assertUniqueSimpleConstantEqualities consSimple = do
  let simplyDefinedVars = concatMap 
        (\case 
          SimpleConstraint x Eq e q -> if Map.null e then [x] else []
          _ -> []
        ) 
        consSimple
  assert
    ("simple constant equalities uniquely define each variable: " ++ show simplyDefinedVars)
    (simplyDefinedVars == List.nub simplyDefinedVars)

fromComplexConstraintsToComplexEqualities :: [Constraint] -> [ComplexEquality]
fromComplexConstraintsToComplexEqualities = fmap f
  where 
    f (ComplexConstraint Eq e q) = ComplexEquality e q
    f (ComplexConstraint Leq e q) = ComplexEquality (e `addExpr` freshVarExpr ()) q
    f (ComplexConstraint Geq e q) = ComplexEquality (e `addExpr` negateExpr (freshVarExpr ())) q

-- makes sure that constant is positive
fromComplexEqualitytoRow :: [Var] -> ComplexEquality -> Row
fromComplexEqualitytoRow vars (ComplexEquality e q) = 
  if q >= 0
    then [ if Map.member v e then e Map.! v else 0 | v <- vars ] <> [q]
    else [ if Map.member v e then -(e Map.! v) else 0 | v <- vars ] <> [-q]

-- outputs a list `vs` that maps indices to variables, and a matrix where column `i` of the matrix corresponds to variable `vs!!i`
fromComplexEqualitiesToMatrix :: [ComplexEquality] -> M ([Var], Mat)
fromComplexEqualitiesToMatrix eqs = do
  let vars = List.nub . concat . fmap (\(ComplexEquality e _) -> Map.keys e) $ eqs
  debug 0 $ "vars = " ++ show vars
  let rows = fromComplexEqualitytoRow vars <$> eqs
  -- get permtuation
  perm <- shuffle [0..length vars - 1]
  -- apply permutation to vars
  vars <- pure $ shuffleBy perm vars 
  -- apply permutation to each row
  rows <- pure $ shuffleBy perm <$> rows
  -- append constant to end of row
  rows <- pure $
    zipWith
      (\row (ComplexEquality _ c) -> row <> [c])
      rows
      eqs

  let mat = rows
  pure (vars, mat)

-- 1 0 0 0 1
-- 0 0 1 0 1
-- 0 0 0 1 1
gaussElim :: Mat -> M Mat
-- gaussElim m = foldM f m [0..min ((nCols m - 1) - 1) (nRows m - 1)] -- don't do very last col
gaussElim m = go m 0
  where 
    go :: Mat -> Int -> M Mat
    go m j | j > min ((nCols m - 1) - 1) (nRows m - 1) = pure m
    go m@rows j | otherwise = do
      debug 1 $ "m = "
      debug 1 $ showMat m
      debug 3 $ "j = " ++ show j

      -- the first j entries are 0
      let numLeading0s row = length $ takeWhile (0 ==) row
      let minNumLeading0s = 
            foldr1 min $
            filter (>= j) $
            fmap numLeading0s rows

      -- if the entire row of vars are zeros, then entire row must be zeros
      -- otherwise there is no soluton because 0x + 0y = 1 cannot be solved
      debug 1 $ "minNumLeading0s = " ++ show minNumLeading0s
      assert
        ("for each row, minNumLeading0s < nCols m - 1 || last row == 0")
        (all (\row -> minNumLeading0s < nCols m - 1 || last row == 0) rows)

      -- choose a row such that...
      i <- choose [0..nRows m - 1]
      let row = getRow i m

      -- in this row, the number of leading 0s must be the min among rows that have a number of leading 0s greater than or equal ot j
      assert
        ("numLeading0s row == minNumLeading0s")
        (numLeading0s row == minNumLeading0s)
      -- note that this shadows the previous value of j
      j <- pure minNumLeading0s

      debug 3 $ "j = " ++ show j
      debug 3 $ "i  = " ++ show i

      debug 1 $ "m!(i="++show i++",j="++show j ++") = " ++ showQ (row!!j)
      
      -- get jth entry
      let a = row!!j
      -- divide row by a
      let row' = (/a) <$> row

      debug 1 $ "row' = " ++ showRow row'

      let 
        -- if jth elem is 0, then pass
        -- if jth elem is non-0, then divide by jth elem then suptract row'
        f :: [Q] -> M [Q]
        f r = 
          let b = r!!j in
            if b == 0 
              then pure r
              else do                
                -- scale `row'` by `b` in order to suptract away entire coefficient for focussed variable
                row' <- pure $ (b*) <$> row'
                -- suptract away row'' from r to eliminate `j` variable coefficient
                debug 3 $ "suptracting " ++ showRow r ++ " by " ++ showRow row'
                r <- pure $ r `subList` row'
                debug 3 $ "result: " ++ showRow r
                debug 3 ""
                -- pure $ (`subList` row') . scaleList (1/b) $ r
                pure r
      -- move row to bottom
      -- for each other row:
      --   if jth elem is 0, then pass
      --   if jth elem is non-0, then divide by jth elem then suptract row'
      m <- do
        rows <- pure m
        rows <- pure $ deleteAtList i rows -- delete row'
        debug 1 $ "normalizing rest of rows"
        debug 1 $ "  row' = " ++ showRow row'
        debug 1 $ "  rows = " ++ show (showRow <$> rows)
        rows <- traverse f rows -- eliminate jth col from other rows
        rows <- pure $ rows <> [row'] -- append row'
        pure rows
      
      -- recurse
      go m (j + 1)

normEchelon :: Mat -> M Mat
normEchelon mat@rows = do
  -- for each index (i,i), if m!(i,i) is negative then negate that row
  debug 1 $ "positive-izing rows"
  debug 1 $ "mat = "
  debug 1 $ showMat mat

  rows <-
    mapM
      (\row -> do
        let a = head . dropWhile (0 ==) $ row
        if a < 0
          then pure $ negate <$> row
          else pure row 
      )
      rows
  pure rows

-- takes mapping `v : Int -> Var` and matrix
-- outputs list of equations, where each equation corresponds to one row of the 
-- outputs a list of equations, where each equation corresponds to a row of the matrix, and column `i` of the matrix corresponds to variable `vs!!i`
fromNormEchelonMatrixToSimpleEqualitys :: [Var] -> Mat -> M [SimpleEquality]
fromNormEchelonMatrixToSimpleEqualitys vars mat@rows = do
  -- equations, but with ints instead of vars
  eqs <-
    mapM
      (\row -> do

        -- row is in the form [0, ..., 0, 1, a1, ..., aN, c]
        -- corresponds to the equation xi = -a1 x{i+1} + ... + -aNx{n} + c
        let c = last row
        -- index of first non-0 elem
        let i = fromJust $ List.findIndex (0 /=) row
        -- extract the vars the are going to be moved to RHS
        -- [0, ..., 0, 0, a1, ..., aN]
        row <-
            zipWithM
              (\j q -> do 
                debug 2 $ "(j, q) = (" ++ show j ++ ", " ++ showQ q ++ ")"
                pure $ if i < j && j < length row - 1 then q else 0
              )
              [0..] row
        -- move to RHS
        row <- pure $ negate <$> row

        debug 2 $ "row = " ++ showRow row
        debug 2 $ "vars = " ++ show vars
        debug 2 $ "zip vars row = " ++ show (zip vars row)
        
        let e = normExpr $ Map.fromList $ zip vars row
        debug 2 $ "e = " ++ showExpr e

        pure $ SimpleEquality (vars!!i) e c
      )
      rows 
  pure eqs

-- good forms:
-- • all vars positive, constant positive
assertGoodFormedSimpleEquality :: SimpleEquality -> M ()
assertGoodFormedSimpleEquality eq@(SimpleEquality x e q) = 
  assert 
    ("good-formed simple equality: " ++ showSimpleEquality eq)
    $ foldr1 (||)
        [ all (0 <=) e && q >= 0
        ]

assertGoodFormedSimpleConstraint :: Constraint -> M ()
assertGoodFormedSimpleConstraint con@(SimpleConstraint x r e q) =
  assert
    ("good-formed simple equality: " ++ showConstraint con)
    $ foldr1 (||)
        [ all (0 <=) e && q >= 0
        ]
assertGoodFormedSimpleConstraint con = 
  assert 
    ("good-formed simple equality: " ++ showConstraint con)
    False

constrainIntegralCoefficients :: [Constraint] -> M [Constraint]
constrainIntegralCoefficients cons = 
  let
    m = Map.filter ((1 /=) . fst) $ foldr fold Map.empty cons
      where
        fold :: Constraint -> Map.Map Var (Int, Var) -> Map.Map Var (Int, Var)
        fold (SimpleConstraint _ _ e _) m =
          let 
            toFactor :: Q -> Int
            toFactor q = if isIntegral q then 1 else denomenatorInt q
          in
            Map.foldrWithKey
              (\x q m ->
                let i1 = denomenatorInt q in
                Map.alter 
                  (\case 
                    Just (i2, x') -> Just (lcm i1 i2, x')
                    Nothing -> Just (i1, freshVar ())
                  ) x m
              ) m e
        fold _ m = m
  in do
    consInt <- foldM
      (\cons con -> case con of
        SimpleConstraint x r e q -> 
          if Map.member x m then
            reject "[constrainIntegralCoefficients] Can't constrain a simply-constrained variable"
          else
            pure $ SimpleConstraint x r (supExprByLcmMap m e) q : cons
        ComplexConstraint _ _ _ -> 
            reject "[constrainIntegralCoefficients] Can't handle ComplexConstraints"
      )
      mempty
      cons
    consSup <-
      pure 
        (fmap
          (\(x, (i, x')) -> 
            SimpleConstraint x Eq (Map.fromList [(x', toRational i)]) 0)
          (Map.toList m)
        )
    pure (consInt <> consSup)
  where 
    supExprByLcmMap :: Map.Map Var (Int, Var) -> Expr -> Expr 
    supExprByLcmMap m e = Map.foldrWithKey fold e m
      where 
        fold x (i, x') e = substituteExpr x (Map.fromList [(x', toRational i)]) e

  
pruneUnusedFreshVars :: [Constraint] -> [Constraint]
pruneUnusedFreshVars cons = filter constrainsUsedVar cons
  where
    -- isFreshVar x => x `elem` usedFreshVras
    constrainsUsedVar (SimpleConstraint x _ _ _) = 
      isFreshVar x ==> (x `elem` usedFreshVars)

    usedFreshVars = concatMap
      (\case 
          SimpleConstraint x r e q -> 
            filter isFreshVar $ Map.keys e
      )
      cons

freeVarsOfConstraints :: [Constraint] -> [Var]
freeVarsOfConstraints cons = List.nub $ filter (not . (`elem` constrainedVars)) vars
  where
    vars = concatMap 
      (\case 
        SimpleConstraint x _ e _ -> x : Map.keys e
        ComplexConstraint _ e _ -> Map.keys e
      )
      cons

    constrainedVars = concatMap
      (\case
        SimpleConstraint x _ _ _ -> [x]
        _ -> []
      )
      cons

varsOfConstraints :: [Constraint] -> [Var]
varsOfConstraints = List.nub . concatMap 
  (\case 
    SimpleConstraint x _ e _ -> x : Map.keys e
    ComplexConstraint _ e _ -> Map.keys e
  )

type VarBound = (Int, Maybe Int)

showUpperBound :: Maybe Int -> String
showUpperBound Nothing = "inf"
showUpperBound (Just i) = show i

minUpperBound :: Maybe Int -> Maybe Int -> Maybe Int
minUpperBound Nothing b = b
minUpperBound a Nothing = a
minUpperBound (Just a) (Just b) = Just (min a b)

defaultBound :: VarBound
defaultBound = (0, Nothing)

varBoundsFromConstraints :: [Constraint] -> Map.Map Var VarBound
varBoundsFromConstraints cons = foldr fold (Map.fromList $ fmap (\x -> (x, defaultBound)) (varsOfConstraints cons)) cons
  where 
    fold :: Constraint -> Map.Map Var (Int, Maybe Int) -> Map.Map Var (Int, Maybe Int)
    fold (SimpleConstraint x r e q) m | isConstantExpr e && r /= Eq =
      case r of
        Leq -> Map.alter (maybe (Just (0, Just (floor q))) 
          (\(lo, up) -> Just (lo, minUpperBound up (Just (floor q))))) x m
        Geq -> Map.alter (maybe (Just defaultBound)
          (\(lo, up) -> Just (max lo (ceiling q), up))) x m
    fold (SimpleConstraint x r e q) m | otherwise =
      Map.delete x m

showVarBounds :: Map.Map Var VarBound -> String
showVarBounds m = 
  List.intercalate ", " $ 
  fmap (\(x, (lo, up)) -> show lo ++ " <= " ++ show x ++ " <= " ++ showUpperBound up) $ 
  Map.toList m