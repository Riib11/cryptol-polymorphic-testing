module Q where 

import GHC.Real
import Control.Monad

type Q = Rational

displayQ :: Rational -> String
displayQ (a :% b) | b == 1 = show a 
displayQ (a :% b) | otherwise = "(" ++ show a ++ "/" ++ show b ++ ")"

isIntegral :: Q -> Bool 
isIntegral (a :% b) = b == 1

numeratorInt :: Q -> Int
numeratorInt (a :% b) = fromIntegral a

denomenator :: Q -> Q
denomenator (a :% b) = fromIntegral b

denomenatorInt :: Q -> Int
denomenatorInt (a :% b) = fromIntegral b

fromRationalToInt :: Q -> Maybe Int
fromRationalToInt q = guard (isIntegral q) >> pure (numeratorInt q)