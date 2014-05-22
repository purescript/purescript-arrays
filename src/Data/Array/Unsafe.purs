module Data.Array.Unsafe where

import Prelude.Unsafe
import Data.Array (length)

head :: forall a. [a] -> a
head (x : _) = x

tail :: forall a. [a] -> [a]
tail (_ : xs) = xs

last :: forall a. [a] -> a
last xs = unsafeIndex xs (length xs - 1)
