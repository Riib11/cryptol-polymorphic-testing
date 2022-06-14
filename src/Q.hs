module Q where 

import GHC.Real

type Q = Rational

showQ :: Rational -> String
showQ (a :% b) | b == 1 = show a 
showQ (a :% b) | otherwise = "(" ++ show a ++ "/" ++ show b ++ ")"

isIntegral :: Q -> Bool 
isIntegral (a :% b) = b == 1

denomenator :: Q -> Q
denomenator (a :% b) = fromIntegral b

denomenatorInt :: Q -> Int
denomenatorInt (a :% b) = fromIntegral b
