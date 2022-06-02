module Z where 

import N

type Z = Int

fromN :: N -> Z
fromN = fromInteger . toInteger

toN :: Z -> N
toN z | z >= 0 = fromIntegral z
toN z | z <  0 = fromIntegral (negate z)