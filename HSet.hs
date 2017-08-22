{-# LANGUAGE TypeFamilies #-}
module HSet where
import Prelude hiding ((**))
import Set
import IntSet
import Group
import Heisenberg
    
newtype HSet = HSet [(Int,Int,IntSet)] deriving (Eq,Ord,Read,Show)

merge' :: [(Int,Int,IntSet)] -> [(Int,Int,IntSet)] -> [(Int,Int,IntSet)]
merge' [] l = l
merge' l [] = l
merge' l1@(v1@(a1,b1,cs1):r1) l2@(v2@(a2,b2,cs2):r2)
       | (a1,b1) < (a2,b2) = v1:merge' r1 l2
       | (a2,b2) < (a1,b1) = v2:merge' l1 r2
       | otherwise = (a1,b1,union cs1 cs2):merge' r1 r2
    
instance Set HSet where
    type Elem HSet = Heisen
    empty = HSet []
    singleton (H a b c) = HSet [(a,b,singleton c)]
    toList (HSet l) = concat (map (\(a,b,cs) -> map (H a b) (toList cs)) l)
    union (HSet l1) (HSet l2) = HSet (merge' l1 l2)
    size (HSet l) = sum (map (\(a,b,cs) -> size cs) l)

translateHS :: Heisen -> HSet -> HSet
translateHS (H a1 b1 c1) (HSet l) =
    HSet $ map (\(a2,b2,cs2) -> (a1+a2,b1+b2,translateIS (c1+a1*b2) cs2)) l

instance Monoid HSet where
    mempty = singleton one
    mappend x y = unions [translateHS x1 y | x1 <- toList x]

balls2, balls3, ballsKnight :: [HSet]
balls2 = one:map ((fromList gen2) **) balls2
balls3 = one:map ((fromList gen3) **) balls3
ballsKnight = one:map ((fromList genKnight) **) ballsKnight

balls :: Gens -> [HSet]
balls gs = let bs = one:map ((fromList gs) **) bs in bs
