module Data.Array
  ( (!!)
  , snoc
  , singleton
  , head
  , last
  , tail
  , init
  , null
  , map
  , mapMaybe
  , catMaybes
  , length
  , findIndex
  , findLastIndex
  , elemIndex
  , elemLastIndex
  , append
  , concat
  , reverse
  , drop
  , take
  , insertAt
  , deleteAt
  , updateAt
  , concatMap
  , filter
  , range
  , zipWith
  , nub
  , nubBy
  , sort
  , sortBy
  , group
  , group'
  , groupBy
  , span
  ) where

import Data.Maybe
import Prelude.Unsafe (unsafeIndex)

infixl 8 !!

(!!) :: forall a. [a] -> Number -> Maybe a
(!!) xs n =
  if n < 0 || n >= (length xs) || isInt n
    then Nothing
    else Just (xs `unsafeIndex` n)
  where
  isInt n = n /= complement (complement n)

foreign import snoc
  "function snoc(l) {\
  \  return function (e) {\
  \    var l1 = l.slice();\
  \    l1.push(e); \
  \    return l1;\
  \  };\
  \}" :: forall a. [a] -> a -> [a]

singleton :: forall a. a -> [a]
singleton a = [a]

head :: forall a. [a] -> Maybe a
head (x : _) = Just x
head _       = Nothing

last :: forall a. [a] -> Maybe a
last (x : []) = Just x
last (_ : xs) = last xs
last _        = Nothing

tail :: forall a. [a] -> Maybe [a]
tail (_ : xs) = Just xs
tail _        = Nothing

init :: forall a. [a] -> Maybe [a]
init [] = Nothing
init xs = Just (slice 0 (length xs - 1) xs)

null :: forall a. [a] -> Boolean
null [] = true
null _  = false

foreign import length
  "function length (xs) {\
  \  return xs.length;\
  \}" :: forall a. [a] -> Number

foreign import findIndex
  "function findIndex (f) {\
  \  return function (arr) {\
  \    for (var i = 0, l = arr.length; i < l; i++) {\
  \      if (f(arr[i])) {\
  \        return i;\
  \      }\
  \    }\
  \    return -1;\
  \  };\
  \}" :: forall a. (a -> Boolean) -> [a] -> Number

foreign import findLastIndex
  "function findLastIndex (f) {\
  \  return function (arr) {\
  \    for (var i = arr.length - 1; i >= 0; i--) {\
  \      if (f(arr[i])) {\
  \        return i;\
  \      }\
  \    }\
  \    return -1;\
  \  };\
  \}" :: forall a. (a -> Boolean) -> [a] -> Number

elemIndex :: forall a. (Eq a) => a -> [a] -> Number
elemIndex x = findIndex ((==) x)

elemLastIndex :: forall a. (Eq a) => a -> [a] -> Number
elemLastIndex x = findLastIndex ((==) x)

foreign import append
  "function append (l1) {\
  \  return function (l2) {\
  \    return l1.concat(l2);\
  \  };\
  \}" :: forall a. [a] -> [a] -> [a]

foreign import concat
  "function concat (xss) {\
  \  var result = [];\
  \  for (var i = 0, l = xss.length; i < l; i++) {\
  \    result.push.apply(result, xss[i]);\
  \  }\
  \  return result;\
  \}" :: forall a. [[a]] -> [a]

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

take :: forall a. Number -> [a] -> [a]
take n = slice 0 n

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
  \      var i = ~~index;\
  \      if (i < 0 || i >= l.length) return l;\
  \      var l1 = l.slice();\
  \      l1[i] = a;\
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

mapMaybe :: forall a b. (a -> Maybe b) -> [a] -> [b]
mapMaybe f = concatMap (maybe [] singleton <<< f)

catMaybes :: forall a. [Maybe a] -> [a]
catMaybes = concatMap (maybe [] singleton)

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

foreign import range
  "function range (start) {\
  \  return function (end) {\
  \    var i = ~~start, e = ~~end;\
  \    var step = i > e ? -1 : 1;\
  \    var result = [i], n = 1;\
  \    while (i !== e) {\
  \      i += step;\
  \      result[n++] = i;\
  \    }\
  \    return result;\
  \  };\
  \}" :: Number -> Number -> [Number]

foreign import zipWith
  "function zipWith (f) {\
  \  return function (xs) {\
  \    return function (ys) {\
  \      var l = xs.length < ys.length ? xs.length : ys.length;\
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
sort xs = sortBy compare xs

sortBy :: forall a. (a -> a -> Ordering) -> [a] -> [a]
sortBy comp xs = sortJS comp' xs
  where
    comp' x y = case comp x y of
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

group :: forall a. (Eq a) => [a] -> [[a]]
group xs = groupBy (==) xs

-- | Performs a sorting first.
group' :: forall a. (Ord a) => [a] -> [[a]]
group' = group <<< sort

groupBy :: forall a. (a -> a -> Boolean) -> [a] -> [[a]]
groupBy = go []
  where
  go :: forall a. [[a]] -> (a -> a -> Boolean) -> [a] -> [[a]]
  go acc _  []     = reverse acc
  go acc op (x:xs) = let sp = span (op x) xs in
                     go ((x:sp.init):acc) op sp.rest

span :: forall a. (a -> Boolean) -> [a] -> { init :: [a], rest :: [a] }
span = go []
  where
  go :: forall a. [a] -> (a -> Boolean) -> [a] -> { init :: [a], rest :: [a] }
  go acc p (x:xs) | p x = go (x:acc) p xs
  go acc _ xs           = { init: reverse acc, rest: xs }

instance functorArray :: Functor [] where
  (<$>) = map

instance applyArray :: Apply [] where
  (<*>) = ap

instance applicativeArray :: Applicative [] where
  pure = singleton

instance bindArray :: Bind [] where
  (>>=) = flip concatMap

instance monadArray :: Monad []

instance semigroupArray :: Semigroup [a] where
  (<>) = append

instance alternativeArray :: Alternative [] where
  empty = []
  (<|>) = append
