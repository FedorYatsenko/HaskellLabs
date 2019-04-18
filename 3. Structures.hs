module Lab4 where

import Data.Char

{-
qsort [] = []
qsort (x:xs)=qsort[y|y<-xs,y<=x]++[x]++qsort[y|y<-xs,y>x]

data Complex = Complex {re::Int, im::Int}
instance Show Complex where
  show (Complex re im) | im >= 0 = show re ++ " + i * " ++ show im
                       | True = show re ++ " - i * " ++ show (-1 * im)

safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead (x:_) = Just x

count xs = foldl f 0 xs
  wheres
    f n (Left _) = n
    f n (Right _) = n + 1
-}

data Rang = Six|Seven|Eight|Nine|Ten|V|Q|K|A deriving (Show, Eq, Ord)
data Suit = Hearts|Tiles|Clover|Pikes deriving (Show, Eq)
data Card = Card {card_suit::Suit, card_rang::Rang} deriving Show

comp :: Card->Card->Bool
comp (Card s1 r1) (Card s2 r2) | s1 /= s2 = False
                               | True = (r1 > r2)

turn cards card_on_table = snd $ foldr f (card_on_table,[]) cards
  where
    f card (card_on_table,acc) | comp card card_on_table=(card_on_table, card:acc)
    f card (card_on_table,acc) | True = (card_on_table, acc)

max_card cards suit = snd $ foldr f (suit, Nothing) cards
  where
    f :: Card->(Suit, Maybe Card)->(Suit, Maybe Card)
    f (Card s1 r1) (suit,Nothing)| s1==suit =(suit,(Just (Card s1 r1)))
    f (Card s1 r1) (suit,Just (Card s2 r2))| s1==suit && r1>r2 =(suit,(Just (Card s1 r1)))
    f _ (suit, max) = (suit, max)

either_sum :: [Either Int String] -> Int
either_sum xs = foldl f 0 xs
  where
    -- f :: Either Int String -> Int -> Int
    f acc (Left x) = x + acc
    f acc (Right x) = acc + read x


