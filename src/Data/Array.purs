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
  , length
  , elem
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
  
foreign import elem
  "function elem(e) {\
  \  return function (l) {\
  \    return l.indexOf(e) !== -1;\
  \  };\
  \}" :: forall a. a -> [a] -> Boolean

foreign import elemIndex
  "function elemIndex (e) {\
  \  return function (l) {\
  \    return l.indexOf(e);\
  \  };\
  \}" :: forall a. a -> [a] -> Number

foreign import elemLastIndex
  "function elemLastIndex (e) {\
  \  return function (l) {\
  \    return l.lastIndexOf(e);\
  \  };\
  \}" :: forall a. a -> [a] -> Number
  
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

range :: Number -> Number -> [Number]
range lo hi | lo > hi = []
range lo hi = lo : range (lo + 1) hi

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
