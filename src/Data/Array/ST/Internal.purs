module Data.Array.ST.Internal where

import Control.Monad.ST (Region)

-- | A reference to a mutable array.
-- |
-- | The first type parameter represents the memory region which the array belongs to.
-- | The second type parameter defines the type of elements of the mutable array.
-- |
-- | The runtime representation of a value of type `STArray h a` is the same as that of `Array a`,
-- | except that mutation is allowed.
foreign import data STArray :: Region -> Type -> Type

type role STArray nominal representational
