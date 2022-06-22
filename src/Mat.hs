{-# LANGUAGE DeriveFunctor #-}

module Mat where

import qualified Data.List as List
import Q
import Utils

data MatF r = Mat [r] deriving (Eq, Functor, Show)
data RowF a = Row [a] a deriving (Eq, Functor, Show)

type Mat = MatF Row
type Row = RowF Q

-- display

displayMat :: Mat -> String
displayMat (Mat rows) | null rows = "[]"
displayMat (Mat rows) | otherwise = unlines $ displayRow <$> rows

displayRow :: Row -> String
displayRow (Row xs c) = displayRowVars xs ++ " = " ++ displayQ c

displayRowVars :: [Q] -> String
displayRowVars xs = List.intercalate " + " 
  ((\(x, j) -> displayQ x ++ displayVar j) 
    <$> zip xs [0..])

displayVar :: Int -> String 
displayVar j = "x" ++ show j

-- utilities

nRows, nCols :: Mat -> Int 
nRows (Mat rows) = length rows 
nCols (Mat []) = 0
nCols (Mat (Row xs c:_)) = length xs

getEntry :: Int -> Int -> Mat -> Q
getEntry i j mat@(Mat rows) = let Row xs c = getRow i mat in xs!!j

rowsMat :: Mat -> [Row]
rowsMat (Mat rows) = rows

getRow :: Int -> Mat -> Row
getRow i (Mat rows) = rows!!i

countLeading0sRows :: Mat -> [Int]
countLeading0sRows (Mat rows) = fmap countLeading0sRow rows 

countLeading0sRow :: Row -> Int
countLeading0sRow (Row xs c) = length . takeWhile (== 0) $ xs

isSolvable :: Mat -> Bool
isSolvable (Mat rows) = all f rows
  where 
    f (Row xs c) = (all (0 ==) xs) ==> (c == 0)

scaleRowMat :: Int -> Q -> Mat -> Mat 
scaleRowMat i x (Mat rows) = Mat $ modifyAtList i (fmap (x *)) rows

subRowMat :: Int -> Row -> Mat -> Mat
subRowMat i row (Mat rows) = Mat $ modifyAtList i (`subRow` row) rows

subRow :: Row -> Row -> Row
subRow (Row xs1 c1) (Row xs2 c2) = Row (zipWith (-) xs1 xs2) (c1 - c2)

swapRowsMat :: Int -> Int -> Mat -> Mat
swapRowsMat i i' (Mat rows) = Mat $ swapAtList i i' rows

pullRowToHeadMat :: Int -> Mat -> Mat 
pullRowToHeadMat i (Mat rows) = Mat $ pullToHead i rows

-- adds another column without affecting indices of other columns
-- this new column corresponds to a fresh var
addEmptyCol :: Mat -> Mat
addEmptyCol (Mat rows) = Mat $ (\(Row xs c) -> Row (xs <> [0]) c) <$> rows

addRow :: Row -> Mat -> Mat
addRow row (Mat rows) = Mat (row : rows)

getCol :: Int -> Mat -> [Q]
getCol j (Mat rows) = [ xs!!j | Row xs _ <- rows ]

nullMat :: Mat -> Bool 
nullMat (Mat rows) = null rows