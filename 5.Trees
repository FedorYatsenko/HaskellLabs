module Lab6 where

data Tree a = Empty | Node a (Tree a) (Tree a) deriving Show

tree1 = Node 4 Empty Empty
tree5 = Node 6 Empty tree1
tree2 = Node 3 Empty Empty
tree3 = Node 2 tree1 tree5
tree4 = Node 1 tree2 tree3

height Empty = 0
height (Node _ tr1 tr2) = max (height tr1) (height tr2) + 1

path Empty = []
path (Node a Empty Empty) = [[a]]
path (Node a tr1 tr2) = map (a:) (path tr1 ++ path tr2)

find el Empty = []
find el (Node a tr1 tr2) | a == el = a:(find el tr1 ++ find el tr2)
                         | True = (find el tr1 ++ find el tr2)

data Rose a = Rose a [Rose a] deriving Show

rose1 = Rose 4 []
rose2 = Rose 5 []
rose3 = Rose 3 []
rose4 = Rose 7 []
rose5 = Rose 2 [rose1, rose2, rose3]
rose6 = Rose 0 []
rose7 = Rose 3 [rose4]
rose8 = Rose 1 [rose5, rose6, rose7]
 
sumRose (Rose a []) = a
sumRose (Rose a rs) = f a rs
  where
    f ac [] = ac
    f ac (r:rs) = f (sumRose r + ac) rs

find2 el (Rose a rs)| a == el = a:(f el rs)
                    | True = (f el rs)
                        where
                          f el [] = []
                          f el (r:rs) = find2 el r ++ f el rs
