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
