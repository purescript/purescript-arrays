-- | Partial helper functions for working with immutable arrays.
module Data.Array.Partial
  ( unsafeIndex
  , head
  , tail
  , last
  , init
  ) where

import Prelude

import Data.Array (length, slice)

-- | Find the element of an array at the specified index.
unsafeIndex :: forall a. Partial => Array a -> Int -> a
unsafeIndex = unsafeIndexImpl

foreign import unsafeIndexImpl :: forall a. Array a -> Int -> a

-- | Get the first element of a non-empty array.
-- |
-- | Running time: `O(1)`.
head :: forall a. Partial => Array a -> a
head xs = unsafeIndex xs 0

-- | Get all but the first element of a non-empty array.
-- |
-- | Running time: `O(n)`, where `n` is the length of the array.
tail :: Partial => Array ~> Array
tail xs = slice 1 (length xs) xs

-- | Get the last element of a non-empty array.
-- |
-- | Running time: `O(1)`.
last :: forall a. Partial => Array a -> a
last xs = unsafeIndex xs (length xs - 1)

-- | Get all but the last element of a non-empty array.
-- |
-- | Running time: `O(n)`, where `n` is the length of the array.
init :: Partial => Array ~> Array
init xs = slice 0 (length xs - 1) xs
