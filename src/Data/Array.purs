module Data.Array where

import Prelude
import Data.Maybe

head :: forall a. [a] -> Maybe a
head (x : _) = Just x
head _ = Nothing

tail :: forall a. [a] -> Maybe [a]
tail (_ : xs) = Just xs
tail _ = Nothing

map :: forall a b. (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs

foreign import length
  "function length(xs) {\
  \  return xs.length;\
  \}" :: forall a. [a] -> Number

foreign import indexOf
  "function indexOf(l) {\
  \  return function (e) {\
  \    return l.indexOf(e);\
  \  };\
  \}" :: forall a. [a] -> a -> Number

foreign import lastIndexOf
  "function lastIndexOf(l) {\
  \  return function (e) {\
  \    return l.lastIndexOf(e);\
  \  };\
  \}" :: forall a. [a] -> a -> Number

foreign import concat
  "function concat(l1) {\
  \  return function (l2) {\
  \    return l1.concat(l2);\
  \  };\
  \}" :: forall a. [a] -> [a] -> [a]

foreign import joinS
  "function joinS(l) {\
  \  return l.join();\
  \}" :: [String] -> String

foreign import joinWith
  "function joinWith(l) {\
  \  return function (s) {\
  \    return l.join(s);\
  \  };\
  \}" :: [String] -> String -> String

foreign import push
  "function push(l) {\
  \  return function (e) {\
  \    var l1 = l.slice();\
  \    l1.push(e); \
  \    return l1;\
  \  };\
  \}" :: forall a. [a] -> a -> [a]

foreign import reverse
  "function reverse(l) {\
  \  var l1 = l.slice();\
  \  l1.reverse(); \
  \  return l1;\
  \}" :: forall a. [a] -> [a]

foreign import shift
  "function shift(l) {\
  \  var l1 = l.slice();\
  \  l1.shift();\
  \  return l1;\
  \}" :: forall a. [a] -> [a]

foreign import slice
  "function slice(s) {\
  \  return function(e) {\
  \    return function (l) {\
  \      return l.slice(s, e);\
  \    };\
  \  };\
  \}" :: forall a. Number -> Number -> [a] -> [a]

foreign import sort
  "function sort(l) {\
  \  var l1 = l.slice();\
  \  l1.sort();\
  \  return l1;\
  \}" :: forall a. [a] -> [a]

foreign import insertAt
  "function insertAt(index) {\
  \  return function(a) {\
  \    return function(l) {\
  \      var l1 = l.slice();\
  \      l1.splice(index, 0, a);\
  \      return l1;\
  \    }; \
  \  };\
  \}":: forall a. Number -> a -> [a] -> [a]

foreign import deleteAt
  "function deleteAt(index) {\
  \  return function(n) {\
  \    return function(l) {\
  \      var l1 = l.slice();\
  \      l1.splice(index, n);\
  \      return l1;\
  \    }; \
  \  };\
  \}":: forall a. Number -> Number -> [a] -> [a]

foreign import updateAt
  "function updateAt(index) {\
  \  return function(a) {\
  \    return function(l) {\
  \      var l1 = l.slice();\
  \      l1[index] = a;\
  \      return l1;\
  \    }; \
  \  };\
  \}":: forall a. Number -> a -> [a] -> [a]

infixr 6 :

(:) :: forall a. a -> [a] -> [a]
(:) a = concat [a]

singleton :: forall a. a -> [a]
singleton a = [a]

concatMap :: forall a b. (a -> [b]) -> [a] -> [b]
concatMap _ [] = []
concatMap f (a:as) = f a `concat` concatMap f as

filter :: forall a. (a -> Boolean) -> [a] -> [a]
filter _ [] = []
filter p (x:xs) | p x = x : filter p xs
filter p (_:xs) = filter p xs

isEmpty :: forall a. [a] -> Boolean
isEmpty [] = true
isEmpty _ = false

range :: Number -> Number -> [Number]
range lo hi | lo > hi = []
range lo hi = lo : range (lo + 1) hi

zipWith :: forall a b c. (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f (a:as) (b:bs) = f a b : zipWith f as bs
zipWith _ _ _ = []

drop :: forall a. Number -> [a] -> [a]
drop 0 xs = xs
drop _ [] = []
drop n (x:xs) = drop (n - 1) xs

take :: forall a. Number -> [a] -> [a]
take 0 _ = []
take _ [] = []
take n (x:xs) = x : take (n - 1) xs

nub :: forall a. (Eq a) => [a] -> [a]
nub = nubBy (==)

nubBy :: forall a. (a -> a -> Boolean) -> [a] -> [a]
nubBy _ [] = []
nubBy (==) (x:xs) = x : nubBy (==) (filter (\y -> not (x == y)) xs)

instance showArray :: (Show a) => Show [a] where
  show xs = "[" ++ joinWith (map show xs) "," ++ "]"

instance eqArray :: (Eq a) => Eq [a] where
  (==) [] [] = true
  (==) (x:xs) (y:ys) = x == y && xs == ys
  (==) _ _ = false
  (/=) xs ys = not (xs == ys)

instance monadArray :: Monad [] where
  return = singleton
  (>>=) = flip concatMap

instance functorArray :: Functor [] where
  (<$>) = map

instance alternativeArray :: Alternative [] where
  empty = []
  (<|>) = concat
