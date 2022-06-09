module Q where 

import GHC.Real

type Q = Rational

showQ :: Rational -> String
showQ (a :% b) | b == 1 = show a 
showQ (a :% b) | otherwise = "(" ++ show a ++ "/" ++ show b ++ ")"