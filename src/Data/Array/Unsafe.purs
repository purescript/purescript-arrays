module Data.Array.Unsafe where

import Prelude.Unsafe
import Data.Maybe.Unsafe
import qualified Data.Array as A

head :: forall a. [a] -> a
head (x : _) = x

tail :: forall a. [a] -> [a]
tail (_ : xs) = xs

last :: forall a. [a] -> a
last xs = unsafeIndex xs (A.length xs - 1)

init :: forall a. [a] -> [a]
init = fromJust <<< A.init

-- The slice method returns the selected elements in an array, as a new array object.
-- It selects the elements starting at the given (1based) start argument, and ends at,
-- but does not include, the given end argument.
foreign import slice
  "function slice (s) {\
  \  return function (e) {\
  \    return function (l) {\
  \      return l.slice(s, e);\
  \    };\
  \  };\
  \}" :: forall a. Number -> Number -> [a] -> [a]
