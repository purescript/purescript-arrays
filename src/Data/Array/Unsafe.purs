-- | Unsafe helper functions for working with immutable arrays.
-- |
-- | _Note_: these functions should be used with care, and may result in unspecified
-- | behavior, including runtime exceptions.

module Data.Array.Unsafe where

import Data.Int (toNumber)
import Data.Maybe.Unsafe (fromJust)
import qualified Data.Array as A

-- | Find the element of an array at the specified index.
-- |
-- | Note: this function can cause unpredictable failure at runtime if the index is out-of-bounds.
foreign import unsafeIndex
  """
  function unsafeIndex(xs) {
    return function(n) {
      return xs[n];
    };
  }
  """ :: forall a. Array a -> Int -> a

-- | Get the first element of a non-empty array.
-- |
-- | Running time: `O(1)`.
head :: forall a. Array a -> a
head xs = unsafeIndex xs 0

-- | Get all but the first element of a non-empty array.
-- |
-- | Running time: `O(n)`, where `n` is the length of the array.
tail :: forall a. Array a -> Array a
tail xs = (fromJust (A.uncons xs)).tail

-- | Get the last element of a non-empty array.
-- |
-- | Running time: `O(1)`.
last :: forall a. Array a -> a
last xs = unsafeIndex xs (A.length xs - one)

-- | Get all but the last element of a non-empty array.
-- |
-- | Running time: `O(n)`, where `n` is the length of the array.
init :: forall a. Array a -> Array a
init = fromJust <<< A.init
