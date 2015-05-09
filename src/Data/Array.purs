-- | Helper functions for working with immutable Javascript arrays.
-- |
-- | _Note_: Depending on your use-case, you may prefer to use `Data.List` or
-- | `Data.Sequence` instead, which might give better performance for certain
-- | use cases. This module is useful when integrating with JavaScript libraries
-- | which use arrays, but immutable arrays are not a practical data structure
-- | for many use cases due to their poor asymptotics.
module Data.Array
  ( (!!)
  , index
  , (..)
  , cons
  , uncons
  , (:)
  , snoc
  , singleton
  , head
  , last
  , tail
  , init
  , null
  , mapMaybe
  , catMaybes
  , length
  , findIndex
  , findLastIndex
  , elemIndex
  , elemLastIndex
  , concat
  , reverse
  , span
  , drop
  , dropWhile
  , take
  , takeWhile
  , insertAt
  , deleteAt
  , updateAt
  , modifyAt
  , delete
  , deleteBy
  , (\\)
  , intersect
  , intersectBy
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
  , replicate
  , foldlArray
  , foldrArray
  ) where

import Control.Alt (Alt)
import Control.Alternative (Alternative)
import Control.MonadPlus (MonadPlus)
import Control.Plus (Plus)
import Data.Function
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), maybe, isJust)
import Data.Monoid (Monoid)

foreign import indexImpl
  "function indexImpl(just, nothing, xs, i) {\
  \  if (i < 0 || i >= xs.length) {\
  \    return nothing;\
  \  } else {\
  \    return just(xs[i]);\
  \  }\
  \}" :: forall a. Fn4 (forall r. r -> Maybe r) 
                       (forall r. Maybe r)
                       (Array a)
                       Int
                       (Maybe a)

-- | This function provides a safe way to read a value at a particular index
-- | from an array.
index :: forall a. Array a -> Int -> Maybe a
index = runFn4 indexImpl Just Nothing 

infixl 8 !!
          
-- | An infix version of `index`.
(!!) :: forall a. Array a -> Int -> Maybe a
(!!) = index

-- | Attaches an element to the front of an array, creating a new array.
-- |
-- | ```purescript
-- | cons 1 [2, 3, 4] = [1, 2, 3, 4]
-- | ```
-- |
-- | Note, the running time of this function is `O(n)`.
foreign import cons
  """
  function cons(e) {
    return function(l) {
      return [e].concat(l);
    };
  }
  """ :: forall a. a -> Array a -> Array a

infixr 6 :

-- | An infix alias for `cons`.
-- |
-- | Note, the running time of this function is `O(n)`.
(:) :: forall a. a -> Array a -> Array a
(:) = cons

-- | Append an element to the end of an array, creating a new array.
foreign import snoc
  """
  function snoc(l) {
    return function (e) {
      var l1 = l.slice();
      l1.push(e);
      return l1;
    };
  }
  """ :: forall a. Array a -> a -> Array a

foreign import unconsImpl
  "function unconsImpl(just, nothing, xs) {\
  \  if (xs.length === 0) {\
  \    return nothing;\
  \  } else {\
  \    return just({ head: xs[0], tail: xs.slice(1) });\
  \  }\
  \}":: forall a. Fn3 (forall r. r -> Maybe r) 
                      (forall r. Maybe r)
                      (Array a)
                      (Maybe { head :: a, tail :: Array a })

-- | Break an array into its first element, and the remaining elements
uncons :: forall a. Array a -> Maybe { head :: a, tail :: Array a }
uncons = runFn3 unconsImpl Just Nothing

-- | Create an array of one element
singleton :: forall a. a -> Array a
singleton a = [a]

-- | Get the first element in an array, or `Nothing` if the array is empty
-- |
-- | Running time: `O(1)`.
head :: forall a. Array a -> Maybe a
head xs = _.head <$> uncons xs

-- | Get the last element in an array, or `Nothing` if the array is empty
-- |
-- | Running time: `O(1)`.
last :: forall a. Array a -> Maybe a
last xs = xs !! (length xs - one)

-- | Get all but the first element of an array, creating a new array, or `Nothing` if the array is empty
-- |
-- | Running time: `O(n)` where `n` is the length of the array
tail :: forall a. Array a -> Maybe (Array a)
tail xs = _.tail <$> uncons xs

-- | Get all but the last element of an array, creating a new array, or `Nothing` if the array is empty.
-- |
-- | Running time: `O(n)` where `n` is the length of the array
init :: forall a. Array a -> Maybe (Array a)
init [] = Nothing
init xs = Just (slice zero (length xs - one) xs)

-- | Test whether an array is empty.
null :: forall a. Array a -> Boolean
null [] = true
null _  = false

-- | Get the number of elements in an array.
foreign import length
  """
  function length (xs) {
    return xs.length;
  }
  """ :: forall a. Array a -> Int

-- | Find the first index for which a predicate holds.
findIndex :: forall a. (a -> Boolean) -> Array a -> Maybe Int
findIndex f xs = runFn4 findIndexJS Just Nothing f xs

foreign import findIndexJS
  """
  function findIndexJS(just, nothing, f, arr) {
    for (var i = 0, l = arr.length; i < l; i++) {
      if (f(arr[i])) return just(i);
    }
    return nothing;
  };
  """ :: forall a. Fn4 (Int -> Maybe Int) (Maybe Int) (a -> Boolean) (Array a) (Maybe Int)

-- | Find the last index for which a predicate holds.
findLastIndex :: forall a. (a -> Boolean) -> Array a -> Maybe Int
findLastIndex f xs = runFn4 findLastIndexJS Just Nothing f xs

foreign import findLastIndexJS
  """
  function findLastIndexJS(just, nothing, f, arr) {
    for (var i = arr.length - 1; i >= 0; i--) {
      if (f(arr[i])) return just(i);
    }
    return nothing;
  }
  """ :: forall a. Fn4 (Int -> Maybe Int) (Maybe Int) (a -> Boolean) (Array a) (Maybe Int)

-- | Find the index of the first element equal to the specified element.
elemIndex :: forall a. (Eq a) => a -> Array a -> Maybe Int
elemIndex x = findIndex ((==) x)

-- | Find the index of the last element equal to the specified element.
elemLastIndex :: forall a. (Eq a) => a -> Array a -> Maybe Int
elemLastIndex x = findLastIndex ((==) x)

-- | Flatten an array of arrays, creating a new array.
foreign import concat
  """
  function concat (xss) {
    var result = [];
    for (var i = 0, l = xss.length; i < l; i++) {
      result.push.apply(result, xss[i]);
    }
    return result;
  }
  """ :: forall a. Array (Array a) -> Array a

-- | Reverse an array, creating a copy
foreign import reverse
  """
  function reverse (l) {
    return l.slice().reverse();
  }
  """ :: forall a. Array a -> Array a

-- | Split an array into two parts:
-- |
-- | 1. the longest initial subarray for which all element satisfy the specified predicate
-- | 2. the remaining elements
-- |
-- | For example,
-- |
-- | ```purescript
-- | span (\n -> n % 2 == 1) [1,3,2,4,5] == { init: [1,3], rest: [2,4,5] }
-- | ```
span :: forall a. (a -> Boolean) -> Array a -> { init :: Array a, rest :: Array a }
span p = go []
  where
  go :: Array a -> Array a -> { init :: Array a, rest :: Array a }
  go acc xs = case uncons xs of
                Just { head: x, tail: xs } | p x -> go (x : acc) xs
                _ -> { init: reverse acc, rest: xs }

-- | Drop a number of elements from the start of an array, creating a new array.
foreign import drop
  """
  function drop (n) {
    return function (l) {
      return n < 1 ? l : l.slice(n);
    };
  }
  """ :: forall a. Int -> Array a -> Array a

-- | Remove the longest initial subarray for which all element satisfy the specified predicate,
-- | creating a new array.
dropWhile :: forall a. (a -> Boolean) -> Array a -> Array a
dropWhile p xs = (span p xs).rest

-- | Keep only a number of elements from the start of an array, creating a new array.
take :: forall a. Int -> Array a -> Array a
take n xs | n < one = []
          | otherwise = slice zero n xs

-- | Calculate the longest initial subarray for which all element satisfy the specified predicate,
-- | creating a new array.
takeWhile :: forall a. (a -> Boolean) -> Array a -> Array a
takeWhile p xs = (span p xs).init

-- | Create a copy of a subarray
foreign import slice
  """
  function slice (s) {
    return function (e) {
      return function (l) {
        return l.slice(s, e);
      };
    };
  }
  """ :: forall a. Int -> Int -> Array a -> Array a

-- | Insert an element at the specified index, creating a new array.
foreign import insertAt
  """
  function insertAt (index) {
    return function (a) {
      return function (l) {
        var l1 = l.slice();
        l1.splice(index, 0, a);
        return l1;
      };
    };
  }
  """ :: forall a. Int -> a -> Array a -> Array a

-- | Delete the element at the specified index, creating a new array.
foreign import deleteAt
  """
  function deleteAt (index) {
    return function (n) {
      return function (l) {
        var l1 = l.slice();
        l1.splice(index, n);
        return l1;
      };
    };
  }
  """ :: forall a. Int -> Int -> Array a -> Array a

-- | Change the element at the specified index, creating a new array.
foreign import updateAt
  """
  function updateAt (i) {
    return function (a) {
      return function (l) {
        if (i < 0 || i >= l.length) return l;
        var l1 = l.slice();
        l1[i] = a;
        return l1;
      };
    };
  }
  """ :: forall a. Int -> a -> Array a -> Array a

-- | Apply a function to the element at the specified index, creating a new array.
modifyAt :: forall a. Int -> (a -> a) -> Array a -> Array a
modifyAt i f xs = maybe xs (\x -> updateAt i (f x) xs) (xs !! i)

-- | Delete the first element of an array which is equal to the specified value,
-- | creating a new array.
delete :: forall a. (Eq a) => a -> Array a -> Array a
delete = deleteBy eq

-- | Delete the first element of an array which matches the specified value, under the
-- | equivalence relation provided in the first argument, creating a new array.
deleteBy :: forall a. (a -> a -> Boolean) -> a -> Array a -> Array a
deleteBy _  _ [] = []
deleteBy eq x ys = maybe ys (\i -> deleteAt i one ys) (findIndex (eq x) ys)

infix 5 \\

-- | Delete the first occurrence of each element in the second array from the first array,
-- | creating a new array.
(\\) :: forall a. (Eq a) => Array a -> Array a -> Array a
(\\) xs ys = go xs ys
  where
  go xs [] = xs
  go [] _  = []
  go xs ys = case uncons ys of
               Just o -> go (delete o.head xs) o.tail

-- | Calculate the intersection of two arrays, creating a new array.
intersect :: forall a. (Eq a) => Array a -> Array a -> Array a
intersect = intersectBy eq

-- | Calculate the intersection of two arrays, using the specified equivalence relation
-- | to compare elements, creating a new array.
intersectBy :: forall a. (a -> a -> Boolean) -> Array a -> Array a -> Array a
intersectBy _  [] _  = []
intersectBy _  _  [] = []
intersectBy eq xs ys = filter (\x -> isJust (findIndex (eq x) ys)) xs

-- | Apply a function to each element in an array, and flatten the results
-- | into a single, new array.
concatMap :: forall a b. (a -> Array b) -> Array a -> Array b
concatMap f xs = xs >>= f

-- | Apply a function to each element in an array, keeping only the results which
-- | contain a value, creating a new array.
mapMaybe :: forall a b. (a -> Maybe b) -> Array a -> Array b
mapMaybe f = concatMap (maybe [] singleton <<< f)

-- | Filter an array of optional values, keeping only the elements which contain
-- | a value, creating a new array.
catMaybes :: forall a. Array (Maybe a) -> Array a
catMaybes = mapMaybe id

-- | Filter an array, keeping the elements which satisfy a predicate function,
-- | creating a new array.
foreign import filter
  """
  function filter (f) {
    return function (arr) {
      var n = 0;
      var result = [];
      for (var i = 0, l = arr.length; i < l; i++) {
        if (f(arr[i])) result[n++] = arr[i];
      }
      return result;
    };
  }
  """ :: forall a. (a -> Boolean) -> Array a -> Array a

-- | Create an array containing a range of integers, including both endpoints.
foreign import range
  """
  function range (start) {
    return function (end) {
      var i = start;
      var step = i > end ? -1 : 1;
      var result = [i], n = 1;
      while (i !== end) {
        i += step;
        result[n++] = i;
      }
      return result;
    };
  }
  """ :: Int -> Int -> Array Int

infix 8 ..

-- | An infix synonym for `range`.
(..) :: Int -> Int -> Array Int
(..) = range

-- | Apply a function to pairs of elements at the same index in two arrays,
-- | collecting the results in a new array.
-- |
-- | If one array is longer, elements will be discarded from the longer array.
-- |
-- | For example
-- |
-- | ```purescript
-- | zipWith (*) [1, 2, 3] [4, 5, 6, 7] == [4, 10, 18]
-- | ```
foreign import zipWith
  """
  function zipWith (f) {
    return function (xs) {
      return function (ys) {
        var l = xs.length < ys.length ? xs.length : ys.length;
        var result = new Array(l);
        for (var i = 0; i < l; i++) {
          result[i] = f(xs[i])(ys[i]);
        }
        return result;
      };
    };
  }
  """ :: forall a b c. (a -> b -> c) -> Array a -> Array b -> Array c

-- | Remove the duplicates from an array, creating a new array.
nub :: forall a. (Eq a) => Array a -> Array a
nub = nubBy eq

-- | Remove the duplicates from an array, where element equality is determined by the
-- | specified equivalence relation, creating a new array.
nubBy :: forall a. (a -> a -> Boolean) -> Array a -> Array a
nubBy _ [] = []
nubBy eq xs = case uncons xs of
                Just o -> o.head : nubBy eq (filter (\y -> not (o.head `eq` y)) o.tail)

-- | Sort the elements of an array in increasing order, creating a new array.
sort :: forall a. (Ord a) => Array a -> Array a
sort xs = sortBy compare xs

-- | Sort the elements of an array in increasing order, where elements are compared using
-- | the specified partial ordering, creating a new array.
sortBy :: forall a. (a -> a -> Ordering) -> Array a -> Array a
sortBy comp xs = sortJS comp' xs
  where
  comp' x y = case comp x y of
    GT -> 1
    EQ -> 0
    LT -> -1

foreign import sortJS
  """
  function sortJS (f) {
    return function (l) {
      return l.slice().sort(function (x, y) {
        return f(x)(y);
      });
    };
  }
  """ :: forall a. (a -> a -> Int) -> Array a -> Array a

-- | Group equal, consecutive elements of an array into arrays.
-- |
-- | For example,
-- |
-- | ```purescript
-- | group [1,1,2,2,1] == [[1,1],[2,2],[1]]
-- | ```
group :: forall a. (Eq a) => Array a -> Array (Array a)
group xs = groupBy eq xs

-- | Sort and group the elements of an array into arrays.
-- |
-- | For example,
-- |
-- | ```purescript
-- | group [1,1,2,2,1] == [[1,1,1],[2,2]]
-- | ```
group' :: forall a. (Ord a) => Array a -> Array (Array a)
group' = group <<< sort

-- | Group equal, consecutive elements of an array into arrays, using the specified
-- | equivalence relation to detemine equality.
groupBy :: forall a. (a -> a -> Boolean) -> Array a -> Array (Array a)
groupBy op = go []
  where
  go :: Array (Array a) -> Array a -> Array (Array a)
  go acc []     = reverse acc
  go acc xs = case uncons xs of
                Just o -> let sp = span (op o.head) o.tail
                          in go ((o.head : sp.init) : acc) sp.rest

-- | Create an array with repeated instances of a value.
foreign import replicate
  """
  function replicate(nn) {
    return function(v) {
      var n = nn > 0 ? nn : 0;
      var r = new Array(n);
      for (var i = 0; i < n; i++) r[i] = v;
      return r;
     };
  }
  """ :: forall a. Int -> a -> Array a

-- | Fold an array from the right
foreign import foldrArray
  """
  function foldrArray(f) {
    return function(z) {
      return function(xs) {
        var acc = z;
        for (var i = xs.length - 1; i >= 0; --i) {
          acc = f(xs[i])(acc);
        }
        return acc;
      };
    };
  }
  """ :: forall a b. (a -> b -> b) -> b -> Array a -> b

-- | Fold an array from the left
foreign import foldlArray
  """
  function foldlArray(f) {
    return function(z) {
      return function(xs) {
        var acc = z;
        for (var i = 0, len = xs.length; i < len; ++i) {
          acc = f(acc)(xs[i]);
        }
        return acc;
      };
    };
  }
  """ :: forall a b. (b -> a -> b) -> b -> Array a -> b

instance altArray :: Alt Array where
  alt = append

instance plusArray :: Plus Array where
  empty = []

instance alternativeArray :: Alternative Array

instance monadPlusArray :: MonadPlus Array
