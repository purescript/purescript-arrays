module Data.Array
  ( (:)
  , singleton
  , head
  , tail
  , map
  , length
  , indexOf
  , lastIndexOf
  , concat
  , joinWith
  , reverse
  , drop
  , take
  , slice
  , insertAt
  , deleteAt
  , updateAt
  , concatMap
  , filter
  , isEmpty
  , range
  , zipWith
  , nub
  , nubBy
  , sort
  ) where

import Prelude
import Data.Maybe

infixr 6 :

(:) :: forall a. a -> [a] -> [a]
(:) a = concat [a]

singleton :: forall a. a -> [a]
singleton a = [a]

head :: forall a. [a] -> Maybe a
head (x : _) = Just x
head _ = Nothing

tail :: forall a. [a] -> Maybe [a]
tail (_ : xs) = Just xs
tail _ = Nothing

foreign import map
  "function map (f) {\
  \  return function (arr) {\
  \    var l = arr.length;\
  \    var result = new Array(l);\
  \    for (var i = 0; i < l; i++) {\
  \      result[i] = f(arr[i]);\
  \    }\
  \    return result;\
  \  };\
  \}" :: forall a b. (a -> b) -> [a] -> [b]

foreign import length
  "function length (xs) {\
  \  return xs.length;\
  \}" :: forall a. [a] -> Number

foreign import indexOf
  "function indexOf (l) {\
  \  return function (e) {\
  \    return l.indexOf(e);\
  \  };\
  \}" :: forall a. [a] -> a -> Number

foreign import elem
  "function elem(l) {\
  \  return function (e) {\
  \    return l.indexOf(e) !== -1;\
  \  };\
  \}" :: forall a. [a] -> a -> Boolean

foreign import lastIndexOf
  "function lastIndexOf (l) {\
  \  return function (e) {\
  \    return l.lastIndexOf(e);\
  \  };\
  \}" :: forall a. [a] -> a -> Number

foreign import concat
  "function concat (l1) {\
  \  return function (l2) {\
  \    return l1.concat(l2);\
  \  };\
  \}" :: forall a. [a] -> [a] -> [a]

foreign import joinWith
  "function joinWith (l) {\
  \  return function (s) {\
  \    return l.join(s);\
  \  };\
  \}" :: [String] -> String -> String

foreign import reverse
  "function reverse (l) {\
  \  return l.slice().reverse();\
  \}" :: forall a. [a] -> [a]

foreign import drop
  "function drop (n) {\
  \  return function (l) {\
  \    return l.slice(n);\
  \  };\
  \}" :: forall a. Number -> [a] -> [a]

foreign import take
  "function take (n) {\
  \  return function (l) {\
  \    return l.slice(0, n);\
  \  };\
  \}" :: forall a. Number -> [a] -> [a]

foreign import slice
  "function slice (s) {\
  \  return function (e) {\
  \    return function (l) {\
  \      return l.slice(s, e);\
  \    };\
  \  };\
  \}" :: forall a. Number -> Number -> [a] -> [a]

foreign import insertAt
  "function insertAt (index) {\
  \  return function (a) {\
  \    return function (l) {\
  \      var l1 = l.slice();\
  \      l1.splice(index, 0, a);\
  \      return l1;\
  \    }; \
  \  };\
  \}":: forall a. Number -> a -> [a] -> [a]

foreign import deleteAt
  "function deleteAt (index) {\
  \  return function (n) {\
  \    return function (l) {\
  \      var l1 = l.slice();\
  \      l1.splice(index, n);\
  \      return l1;\
  \    }; \
  \  };\
  \}":: forall a. Number -> Number -> [a] -> [a]

foreign import updateAt
  "function updateAt (index) {\
  \  return function (a) {\
  \    return function (l) {\
  \      var l1 = l.slice();\
  \      l1[index] = a;\
  \      return l1;\
  \    }; \
  \  };\
  \}":: forall a. Number -> a -> [a] -> [a]

foreign import concatMap
  "function concatMap (f) {\
  \  return function (arr) {\
  \    var result = [];\
  \    for (var i = 0, l = arr.length; i < l; i++) {\
  \      Array.prototype.push.apply(result, f(arr[i]));\
  \    }\
  \    return result;\
  \  };\
  \}" :: forall a b. (a -> [b]) -> [a] -> [b]

foreign import filter
  "function filter (f) {\
  \  return function (arr) {\
  \    var n = 0;\
  \    var result = [];\
  \    for (var i = 0, l = arr.length; i < l; i++) {\
  \      if (f(arr[i])) {\
  \        result[n++] = arr[i];\
  \      }\
  \    }\
  \    return result;\
  \  };\
  \}" :: forall a. (a -> Boolean) -> [a] -> [a]

isEmpty :: forall a. [a] -> Boolean
isEmpty [] = true
isEmpty _ = false

range :: Number -> Number -> [Number]
range lo hi | lo > hi = []
range lo hi = lo : range (lo + 1) hi

foreign import zipWith
  "function zipWith (f) {\
  \  return function (xs) {\
  \    return function (ys) {\
  \      var l = xs.length > ys.length ? xs.length : ys.length;\
  \      var result = new Array(l);\
  \      for (var i = 0; i < l; i++) {\
  \        result[i] = f(xs[i])(ys[i]);\
  \      }\
  \      return result;\
  \    };\
  \  };\
  \}" :: forall a b c. (a -> b -> c) -> [a] -> [b] -> [c]

nub :: forall a. (Eq a) => [a] -> [a]
nub = nubBy (==)

nubBy :: forall a. (a -> a -> Boolean) -> [a] -> [a]
nubBy _ [] = []
nubBy (==) (x:xs) = x : nubBy (==) (filter (\y -> not (x == y)) xs)

sort :: forall a. (Ord a) => [a] -> [a]
sort xs = sortJS comp xs
  where
  comp x y = case compare x y of
    GT -> 1
    EQ -> 0
    LT -> -1

foreign import sortJS
  "function sortJS (f) {\
  \  return function (l) {\
  \    return l.slice().sort(function (x, y) {\
  \      return f(x)(y);\
  \    });\
  \  };\
  \}" :: forall a. (a -> a -> Number) -> [a] -> [a]

instance showArray :: (Show a) => Show [a] where
  show xs = "[" ++ joinWith (map show xs) "," ++ "]"

instance monadArray :: Monad [] where
  return = singleton
  (>>=) = flip concatMap
  
instance applicativeArray :: Applicative [] where
  pure = return
  (<*>) = ap

instance functorArray :: Functor [] where
  (<$>) = map

instance alternativeArray :: Alternative [] where
  empty = []
  (<|>) = concat
