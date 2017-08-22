{-# LANGUAGE TypeFamilies #-}
module IntSet(IntSet(..),translateIS) where
import Set

newtype IntSet = IntSet {unIS :: [(Int, Int)]} deriving (Eq,Ord,Read,Show)

merge', merge'' :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
merge' [] l = l
merge' l [] = l
merge' l1@((a1,_):_) l2@((a2,_):_) | a1 < a2 = merge'' l1 l2
                                   | otherwise = merge'' l2 l1

merge'' l1@((a1,b1):r1) l2@((a2,b2):r2)
    | b1 < a2   = (a1,b1):merge' r1 l2
    | b1 < b2   = merge' ((a1,b2):r2) r1
    | otherwise = merge' l1 r2

instance Set IntSet where
    type Elem IntSet = Int
    empty = IntSet []
    singleton x = IntSet [(x,x+1)]
    toList (IntSet []) = []
    toList (IntSet ((x,y):r)) = [x..y-1] ++ toList (IntSet r)
    union (IntSet l1) (IntSet l2) = IntSet (merge' l1 l2)
    size (IntSet []) = 0
    size (IntSet ((x,y):r)) = y - x + size (IntSet r)

translateIS :: Int -> IntSet -> IntSet
translateIS x (IntSet l) = IntSet $ map (\(a,b) -> (a+x,b+x)) l
