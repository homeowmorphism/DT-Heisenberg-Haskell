module Group where
import Prelude hiding ((**))
infixl 7 **    

one :: (Monoid m) => m
one = mempty
(**) :: (Monoid m) => m -> m -> m
(**) = mappend

class (Monoid g) => Group g where
    inverse :: g -> g
