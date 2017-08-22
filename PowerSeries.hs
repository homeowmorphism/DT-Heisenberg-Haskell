module PowerSeries where
import Util
import Prelude hiding ((*>))

infixr 7 *>
    
newtype PowerSeries a = PS {coeffs :: [a]} deriving (Eq, Ord, Read, Show)

(*>) :: (Num a) => a -> PowerSeries a -> PowerSeries a
x *> (PS ys) = PS (map (x*) ys)

constant :: a -> PowerSeries a
constant x = PS [x]

instance (Num a) => Num (PowerSeries a) where
    PS xs + PS ys = PS (zipWithDefaults (+) 0 0 xs ys)
    fromInteger = constant . fromInteger
    PS [] * _ = PS []
    PS (x0:xs) * y = let PS zs = PS xs * y in x0 *> y + PS (0:zs)
    negate (PS xs) = PS (map negate xs)
    abs = error "stupid type class"
    signum = error "stupid type class"
