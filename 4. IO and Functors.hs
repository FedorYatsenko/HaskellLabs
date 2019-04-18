module Lab5 where

import Data.Char

data Expr = Num Integer | Add Expr Expr | Div Expr Expr deriving Eq

eval (Num a) = Just a
eval (Add x y) = (+) <$> eval x <*> eval y
eval (Div x y) | y == (Num 0) = Nothing
               | True = div <$> eval x <*> eval y

op_count (Num a) = 0
op_count (Add x y) = op_count x + op_count y + 1
op_count (Div x y) = op_count x + op_count y + 1

{-
sh (Num a) = a
sh (Add x y) = sh x ++ "+" ++ sh y
sh (Mul x y) = f x ++ "*" ++ f y 
  where f (Add x y) = "(" ++ sh (Add x y) ++ ")"
        f a = sh a
-}

main = do
  putStrLn "Input a"
  a <- readLn
  putStrLn "Input b"
  b <- readLn
  putStr "a + b = "
  let s = a + b
  putStrLn $ show s
  putStr "a * b = "
  let s = a * b
  putStrLn $ show s

copy_file = do
  putStrLn "Input file:"
  fileIn <- getLine
  putStrLn "Output file:"
  fileOut <- getLine
  putStrLn ("Copy " ++ fileIn ++ " in " ++ fileOut)
  readFile fileIn >>= writeFile fileOut
  putStrLn "complite"
  
safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead (x:_) = Just x


find = f 5
  where
    f x = do
      putStrLn (show x)
      s <- getLine
      if s == "l" then
        f (x-1)
      else
        if s == "h" then
          f (x+1)
        else
          putStrLn ("Your num = " ++ show x)    
  