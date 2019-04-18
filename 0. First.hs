module FirstModule where

import Data.Char

f1:: Bool->Bool->Bool
f1 a b = a/=b 


f2 x = x*(8 + mod x 2)

f3 x = if ord x >= ord 'A' && ord x <= ord 'Z'
       then chr(ord x + ord 'a' - ord 'A')
       else x