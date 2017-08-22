module Heisenberg(Heisen(..),gen2,gen3,genKnight,makeSym,Gens) where
import Prelude hiding ((**))
import qualified Data.Set as Set
import Data.Set(Set)
import Group
    
data Heisen = H !Int !Int !Int deriving (Eq, Ord, Read, Show)

instance Monoid Heisen where
    mappend (H a1 b1 c1) (H a2 b2 c2) = H (a1+a2) (b1+b2) (c1+c2+a1*b2)
    mempty = H 0 0 0

instance Group Heisen where
    inverse (H a b c) = H (-a) (-b) (-c+a*b)

type Gens = [Heisen]

makeSym :: Gens -> Gens
makeSym l = [one] ++ map inverse l ++ l
                        
gen2, gen3, genKnight :: [Heisen]
gen2 = makeSym [H 1 0 0,H 0 1 0]
gen3 = makeSym [H 1 0 0,H 0 1 0,H 1 1 1]
genKnight = makeSym [H 2 1 1, H 1 2 1, H 2 (-1) (-1), H 1 (-2) (-1)]

minkowski :: Set Heisen -> Set Heisen -> Set Heisen
minkowski xs ys =
    Set.unions [Set.mapMonotonic (x **) ys | x <- Set.toList xs]

balls2, balls3 :: [Set Heisen]
balls2 = (Set.fromList [one]):map (minkowski (Set.fromList gen2)) balls2
balls3 = (Set.fromList [one]):map (minkowski (Set.fromList gen3)) balls3

