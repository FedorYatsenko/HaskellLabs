module TupleModule where

f xs = f2 0 0 xs
f2 n sum [] = (n, sum)
f2 n sum (x:xs) | x > 0 = f2 (n + 1) sum xs 
                | True = f2 n (sum + x) xs

pow2222 x n = pow x n 1
pow x 0 res = res
pow x n res = pow x (n-1) (res*x)

pow2 x 0 = 1
pow2 x n = x * pow2 x (n-1)

(!^) x n = pow21 x n
pow21 x 0 = 1
pow21 x n = case mod n 2 of
                0 -> (pow21 x (div n 2)) * (pow21 x (div n 2))
                _ -> x*(pow21 x (n-1))

mysum (x:xs) = mysum2 xs x
mysum2 [] sum = [sum]
mysum2 (x:xs) sum = sum : mysum2 xs (x + sum) 
