module Data.Array.Unsafe where

head :: forall a. [a] -> a
head (x : _) = x

tail :: forall a. [a] -> [a]
tail (_ : xs) = xs
