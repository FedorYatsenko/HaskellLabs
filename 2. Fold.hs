module Lab3 where

import Data.Char

my_sum xs = foldl (+) 0 xs

my_length xs = foldl (\ ac x -> ac + 1) 0 xs

p x = x > 4
f x = 2 * x

stf x = until p f x         -- 1 -> 1 2 4 8 -> 8 
febf xs = takeWhile p xs    -- [5, 6, 1, 2, 5, 6] -> [5, 6]
dwebf xs = dropWhile p xs   -- [5, 6, 1, 2, 5, 6] -> [1, 2, 5, 6]

inv_list xs = foldl (\ ac x -> x:ac) [] xs

sum_posit_list xs = foldl f 0 xs
  where
    f ac x | x < 0 = ac
    f ac x | True = x + ac
 
clear_list_rev xs = foldl f [] xs
  where
    f ac x | x < 0 = ac
    f ac x | True = x:ac

clear_list xs = foldr f [] xs
  where
    f x ac | x < 0 = ac
    f x ac | True = x:ac

list_to_list [] = [] 
list_to_list (x:xs) = f x xs
  where
    f prev [] = [] 
    f prev (x:xs) = (prev,x):f x xs

--find n xs = foldl f 0 xs
--  where
--    f ac x | x == n = ac + 1
--    f ac x | True = ac
--
zip_3 [] _ _ = []
zip_3 _ [] _ = []
zip_3 _ _ [] = []
-- zip_3 f (x:xs) (y:ys) (z:zs) = f x y z:zip3 xs ys zs
zip_3 (x:xs) (y:ys) (z:zs) = (x+y+z):zip_3 xs ys zs

-- trib = 1:1:zip_3 (+) trib (tail trib) (tail(tail trib))
trib = 1:1:1:zip_3 trib (tail trib) (tail(tail trib))

round_100 a = fromIntegral(round(a * 100.0))/100.0

clear_string xs = snd $ foldr f (True, "") xs
  where
    f x (fg, ac) | (fg && (x /= ' ')) = (True, ac)
    f x (fg, ac) | True = (False, x:ac)

del_double xs = foldr f [] xs
  where
    f x ac | notElem x ac = x:ac
    f x ac | True = ac

find n xs = snd $ foldl f (0,[]) xs
 where
    f (ind, ac) x | x == n = (ind + 1, ind:ac)
    f (ind, ac) x | True = (ind + 1, ac)