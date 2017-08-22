{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Set where
import Group
    
-- No intersection, since we happen not to need it
class Set s where
    type Elem s :: *
    empty :: s
    singleton :: Elem s -> s
    fromList :: [Elem s] -> s
    fromList l = unions (map singleton l)
    toList :: s -> [Elem s]
    union :: s -> s -> s
    unions :: [s] -> s
    unions = foldl union empty
    size :: s -> Int
