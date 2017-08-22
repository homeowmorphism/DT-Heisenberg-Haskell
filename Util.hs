module Util where

diff :: [Int] -> [Int]
diff l = zipWith (-) (tail l) l

groupsOf :: Int -> [a] -> [[a]]
groupsOf n l = let (a,b) = splitAt n l in a:groupsOf n b
    
zipWithDefaults :: (a -> b -> c) -> a -> b -> [a] -> [b] -> [c]
zipWithDefaults f x0 y0 [] ys = zipWith f (repeat x0) ys
zipWithDefaults f x0 y0 xs [] = zipWith f xs (repeat y0)
zipWithDefaults f x0 y0 (x1:xs) (y1:ys) = f x1 y1:zipWithDefaults f x0 y0 xs ys
