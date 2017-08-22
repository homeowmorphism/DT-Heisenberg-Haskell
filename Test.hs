module Test where
import Heisenberg
import HSet
import Group
import Set
import Util
import PowerSeries
import Data.List.Split

sizes :: Gens -> PowerSeries Int
sizes gens = PS (map size (balls gens))

phi :: Int -> PowerSeries Int
phi 1 = PS [1,-1]
phi 2 = PS [1,1]
phi 3 = PS [1,1,1]
phi 4 = PS [1,0,1]
phi 5 = PS [1,1,1,1,1]
phi 6 = PS [1,-1,1]
phi 7 = PS [1,1,1,1,1,1,1]
phi 8 = PS [1,0,0,0,1]
phi 9 = PS [1,0,0,1,0,0,1]
phi 10 = PS [1,-1,1,-1,1]

-- A test family of generators with some interesting behaviours
g :: Int -> Int -> Gens
g k x = makeSym [H 1 0 0, H 0 1 0, H 1 k x]

{-
Sample runs:
λ> sizes gen2 * (phi 1)^5 * phi 3 * phi 4
PS {coeffs = [1,1,4,11,8,21,6,9,1,0,0,...]}
λ> sizes gen3 * (phi 1)^5 * phi 2 * phi 3 * phi 4 * phi 5 * phi 6
PS {coeffs = [1,4,14,42,63,113,139,184,164,179,131,103,54,34,10,1,0,0,0,...]}
λ> sizes (g 2 1) * (phi 1)^5 * phi 3
PS {coeffs = [1,3,11,36,33,33,7,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,...]}
λ> sizes (g 2 0) * (phi 1)^5 * phi 3 * phi 4
PS {coeffs = [1,3,12,35,48,69,44,31,-1,8,-2,2,0,-4,2,0,0,0,0,0,0,0,0,0,...]}
λ> sizes (g 3 1) * (phi 1)^5* (phi 2)^2 * (phi 3)^2 * phi 4 * phi 5 * (phi 6)^2
PS {coeffs = [1,5,25,103,304,651,1193,1889,2669,3426,3954,4154,4045,3551,2837,2085,1328,767,353,145,11,-6,-10,0,-4,-6,10,0,0,0,0,0,0,...]}
λ> sizes (g 4 2) * (phi 1)^5 * (phi 8)^2 * (phi 4)^2 * phi 3 * phi 2
PS {coeffs = [1,4,20,81,246,482,738,980,1212,1434,1658,1794,1742,1508,1186,988,777,614,296,113,-22,10,28,16,-6,-36,-6,-4,18,0,0,0,0,...]}
λ> sizes (g 5 2) * (phi 1)^5 * (phi 10)^2 * phi 8 * (phi 5)^2 * phi 4 * phi 3 * (phi 2)^2
PS {coeffs = [1,5,25,112,394,1100,2212,3730,5521,7683,10273,13152,16178,19100,21542,23302,24070,24312,23586,22674,20349,18035,14585,11938,8896,6900,4676,3126,1725,691,253,-92,100,-58,36,-150,-30,-56,16,24,32,32,0,0,0,0,0,0,0,0,0,Interrupted.
-}
