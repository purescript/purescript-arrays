module Data.Array.ST.Unsafe where

import Control.Monad.ST (ST)
import Data.Array.ST.Internal (STArray)

-- | O(1). Convert a mutable array to an immutable array, without copying. The mutable
-- | array must not be mutated afterwards.
foreign import unsafeFreeze :: forall h a. STArray h a -> ST h (Array a)

-- | O(1) Convert an immutable array to a mutable array, without copying. The input
-- | array must not be used afterward.
foreign import unsafeThaw :: forall h a. Array a -> ST h (STArray h a)
