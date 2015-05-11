-- | Unsafe helper functions for working with immutable arrays.
-- |
-- | _Note_: these functions should be used with care, and may result in unspecified
-- | behavior, including runtime exceptions.

module Data.Array.Unsafe where

import Data.Array (length, slice)

-- | Find the element of an array at the specified index.
-- |
-- | Note: this function can cause unpredictable failure at runtime if the index is out-of-bounds.
foreign import unsafeIndex :: forall a. Array a -> Int -> a

-- | Get the first element of a non-empty array.
-- |
-- | Running time: `O(1)`.
head :: forall a. Array a -> a
head xs = unsafeIndex xs 0

-- | Get all but the first element of a non-empty array.
-- |
-- | Running time: `O(n)`, where `n` is the length of the array.
tail :: forall a. Array a -> Array a
tail xs = slice 1 (length xs) xs

-- | Get the last element of a non-empty array.
-- |
-- | Running time: `O(1)`.
last :: forall a. Array a -> a
last xs = unsafeIndex xs (length xs - 1)

-- | Get all but the last element of a non-empty array.
-- |
-- | Running time: `O(n)`, where `n` is the length of the array.
init :: forall a. Array a -> Array a
init xs = slice 0 (length xs - 1) xs
