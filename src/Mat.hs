module Mat where

import qualified Data.List as List
import Q
import Utils

type Mat = [Row]
type Row = [Q]
type Ix = (Int,Int)

showRow :: Row -> String 
showRow r = "[ " ++ List.intercalate ", " (showQ <$> r) ++ " ]"

showIx :: Ix -> String
showIx (i,j) = "m" ++ show (i,j)

showMat rows = 
  "( " ++ List.intercalate "\n  " [ unwords (pad . showQ <$> row) | row <- rows ] ++ " )"
  where 
    lens = length . showQ <$> concat rows
    max_len = foldr max 0 lens
    pad str = str ++ replicate (max 0 (max_len - length str)) ' '

-- ith row, jth col
(!) :: Mat -> Ix -> Q
m ! (i, j) = (m !! i) !! j

modify :: Ix -> (Q -> Q) -> Mat -> Mat
modify (i, j) f m = modifyList i (modifyList j f) m

modifyList :: Int -> (a -> a) -> [a] -> [a]
modifyList _ f [] = []
modifyList 0 f (x:xs) = f x : xs
modifyList i f (x:xs) = x : modifyList (i-1) f xs

deleteAtList :: Int -> [a] -> [a]
deleteAtList _ [] = []
deleteAtList 0 (x:xs) = xs
deleteAtList i (x:xs) = x : deleteAtList (i-1) xs

-- all the rows have the same length
wellFormed :: Mat -> Bool
wellFormed [] = True
wellFormed (row : rows) = all ((length row ==) . length) rows

nRows, nCols :: Mat -> Int 
nRows rows = length rows 
nCols [] = 0
nCols (row:_) = length row

-- utilities

getRow :: Int -> Mat -> Row
getRow i rows = rows !! i

-- subtract `row` from row `i` in `m`
subRow :: Row -> Int -> Mat -> Mat
subRow row i rows =
  modifyList i 
    (zipWith (\b a -> a - b) row) 
    rows

-- adds `row` to the ith row in `m`
addRow :: Row -> Int -> Mat -> Mat
addRow row i rows =
  modifyList i 
    (zipWith (+) row)
    rows

-- scale by `q` the row `i` in `m`
scaleRow :: Q -> Int -> Mat -> Mat
scaleRow q i rows = 
  modifyList i
    ((q*) <$>) 
    rows

-- moves the ith row to the bottom
moveRowToBot :: Int -> Mat -> Mat
moveRowToBot i rows = 
  let row = rows !! i in
    deleteAtList i rows ++ [row]

deleteRow :: Int -> Mat -> Mat
deleteRow i rows = deleteAtList i rows

appendRow :: Mat -> Row -> Mat
appendRow rows row = rows <> [row]

lastRow :: Mat -> Row
lastRow rows = rows !! (length rows - 1)

initRows :: Mat -> [Row]
initRows rows = init rows

-- r1 - r2
subList :: Row -> Row -> Row
subList r1 r2 = zipWith (-) r1 r2

scaleList :: Q -> Row -> Row
scaleList q r = fmap (q*) r

-- negates all coefficients but doesn't negate constant
negateVars :: Row -> Row
negateVars qs = (negate <$> init qs) <> [last qs]

-- -- zeros out all 
-- zeroExcept :: Int -> Row -> Row
-- zeroExcept 