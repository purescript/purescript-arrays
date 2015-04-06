-- | Unsafe helper functions for working with immutable arrays.
-- |
-- | _Note_: these functions should be used with care, and may result in unspecified
-- | behavior, including runtime exceptions.

module Data.Array.Unsafe where

import Data.Int (Int(), toNumber)
import Data.Maybe.Unsafe (fromJust)
import Prelude.Unsafe (unsafeIndex)
import qualified Data.Array as A

-- | Get the first element of a non-empty array.
-- |
-- | Running time: `O(1)`.
head :: forall a. [a] -> a
head xs = unsafeIndex xs 0

-- | Get all but the first element of a non-empty array.
-- |
-- | Running time: `O(n)`, where `n` is the length of the array.
tail :: forall a. [a] -> [a]
tail (_ : xs) = xs

-- | Get the last element of a non-empty array.
-- |
-- | Running time: `O(1)`.
last :: forall a. [a] -> a
last xs = unsafeIndex xs (toNumber (A.length xs - one))

-- | Get all but the last element of a non-empty array.
-- |
-- | Running time: `O(n)`, where `n` is the length of the array.
init :: forall a. [a] -> [a]
init = fromJust <<< A.init
