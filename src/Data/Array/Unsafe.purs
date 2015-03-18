-- | Unsafe helper functions for working with immutable arrays.
-- | 
-- | _Note_: these functions should be used with care, and may result in unspecified
-- | behavior, including runtime exceptions.

module Data.Array.Unsafe where

import Prelude.Unsafe
import Data.Maybe.Unsafe
import qualified Data.Array as A

-- | Get the first element of a non-empty array.
head :: forall a. [a] -> a
head xs = unsafeIndex xs 0

-- | Get all but the first element of a non-empty array.
tail :: forall a. [a] -> [a]
tail (_ : xs) = xs

-- | Get the last element of a non-empty array.
last :: forall a. [a] -> a
last xs = unsafeIndex xs (A.length xs - 1)

-- | Get all but the last element of a non-empty array.
init :: forall a. [a] -> [a]
init = fromJust <<< A.init
