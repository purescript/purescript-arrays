-- | Helper functions for working with immutable Javascript arrays.
-- |
-- | _Note_: Depending on your use-case, you may prefer to use `Data.List` or
-- | `Data.Sequence` instead, which might give better performance for certain
-- | use cases. This module is useful when integrating with JavaScript libraries
-- | which use arrays, but immutable arrays are not a practical data structure
-- | for many use cases due to their poor asymptotics.
-- |
-- | In addition to the functions in this module, Arrays have a number of
-- | useful instances:
-- |
-- | * `Functor`, which provides `map :: forall a b. (a -> b) -> Array a ->
-- |   Array b`
-- | * `Apply`, which provides `(<*>) :: forall a b. Array (a -> b) -> Array a
-- |   -> Array b`. This function works a bit like a Cartesian product; the
-- |   result array is constructed by applying each function in the first
-- |   array to each value in the second, so that the result array ends up with
-- |   a length equal to the product of the two arguments' lengths.
-- | * `Bind`, which provides `(>>=) :: forall a b. (a -> Array b) -> Array a
-- |   -> Array b` (this is the same as `concatMap`).
-- | * `Semigroup`, which provides `(<>) :: forall a. Array a -> Array a ->
-- |   Array a`, for concatenating arrays.
-- | * `Foldable`, which provides a slew of functions for *folding* (also known
-- |   as *reducing*) arrays down to one value. For example,
-- |   `Data.Foldable.or` tests whether an array of `Boolean` values contains
-- |   at least one `true` value.
-- | * `Traversable`, which provides the PureScript version of a for-loop,
-- |   allowing you to iterate over an array and accumulate effects.
-- |
module Data.Array
  ( fromFoldable
  , toUnfoldable
  , singleton
  , (..), range
  , replicate
  , some
  , many

  , null
  , length

  , (:), cons
  , snoc
  , insert
  , insertBy

  , head
  , last
  , tail
  , init
  , uncons
  , unsnoc

  , (!!), index
  , elemIndex
  , elemLastIndex
  , findIndex
  , findLastIndex
  , insertAt
  , deleteAt
  , updateAt
  , updateAtIndices
  , modifyAt
  , modifyAtIndices
  , alterAt

  , reverse
  , concat
  , concatMap
  , filter
  , partition
  , filterA
  , mapMaybe
  , catMaybes
  , mapWithIndex

  , sort
  , sortBy
  , sortWith
  , slice
  , take
  , takeEnd
  , takeWhile
  , drop
  , dropEnd
  , dropWhile
  , span
  , group
  , group'
  , groupBy

  , nub
  , nubBy
  , union
  , unionBy
  , delete
  , deleteBy

  , (\\), difference
  , intersect
  , intersectBy

  , zipWith
  , zipWithA
  , zip
  , unzip

  , foldM
  , foldRecM

  , unsafeIndex

  , module Exports
  ) where

import Prelude
import Control.Alt ((<|>))
import Control.Alternative (class Alternative)
import Control.Lazy (class Lazy, defer)
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM2)
import Control.Monad.ST (pureST)
import Data.Array.ST (unsafeFreeze, emptySTArray, pokeSTArray, pushSTArray, modifySTArray, withArray)
import Data.Array.ST.Iterator (iterator, iterate, pushWhile)
import Data.Foldable (class Foldable, foldl, foldr, traverse_)
import Data.Foldable (foldl, foldr, foldMap, fold, intercalate, elem, notElem, find, findMap, any, all) as Exports
import Data.Maybe (Maybe(..), maybe, isJust, fromJust)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Traversable (scanl, scanr) as Exports
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..), uncurry)
import Data.Unfoldable (class Unfoldable, unfoldr)
import Partial.Unsafe (unsafePartial)

-- | Convert an `Array` into an `Unfoldable` structure.
toUnfoldable :: forall f. Unfoldable f => Array ~> f
toUnfoldable xs = unfoldr f 0
  where
  len = length xs
  f i
    | i < len   = Just (Tuple (unsafePartial (unsafeIndex xs i)) (i+1))
    | otherwise = Nothing

-- | Convert a `Foldable` structure into an `Array`.
fromFoldable :: forall f. Foldable f => f ~> Array
fromFoldable = fromFoldableImpl foldr

foreign import fromFoldableImpl
  :: forall f a
   . (forall b. (a -> b -> b) -> b -> f a -> b)
  -> f a
  -> Array a

-- | Create an array of one element
singleton :: forall a. a -> Array a
singleton a = [a]

-- | Create an array containing a range of integers, including both endpoints.
foreign import range :: Int -> Int -> Array Int

-- | Create an array containing a value repeated the specified number of times.
foreign import replicate :: forall a. Int -> a -> Array a

-- | An infix synonym for `range`.
infix 8 range as ..

-- | Attempt a computation multiple times, requiring at least one success.
-- |
-- | The `Lazy` constraint is used to generate the result lazily, to ensure
-- | termination.
some :: forall f a. Alternative f => Lazy (f (Array a)) => f a -> f (Array a)
some v = (:) <$> v <*> defer (\_ -> many v)

-- | Attempt a computation multiple times, returning as many successful results
-- | as possible (possibly zero).
-- |
-- | The `Lazy` constraint is used to generate the result lazily, to ensure
-- | termination.
many :: forall f a. Alternative f => Lazy (f (Array a)) => f a -> f (Array a)
many v = some v <|> pure []

--------------------------------------------------------------------------------
-- Array size ------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Test whether an array is empty.
null :: forall a. Array a -> Boolean
null xs = length xs == 0

-- | Get the number of elements in an array.
foreign import length :: forall a. Array a -> Int

--------------------------------------------------------------------------------
-- Extending arrays ------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Attaches an element to the front of an array, creating a new array.
-- |
-- | ```purescript
-- | cons 1 [2, 3, 4] = [1, 2, 3, 4]
-- | ```
-- |
-- | Note, the running time of this function is `O(n)`.
foreign import cons :: forall a. a -> Array a -> Array a

-- | An infix alias for `cons`.
-- |
-- | Note, the running time of this function is `O(n)`.
infixr 6 cons as :

-- | Append an element to the end of an array, creating a new array.
foreign import snoc :: forall a. Array a -> a -> Array a

-- | Insert an element into a sorted array.
insert :: forall a. Ord a => a -> Array a -> Array a
insert = insertBy compare

-- | Insert an element into a sorted array, using the specified function to
-- | determine the ordering of elements.
insertBy :: forall a. (a -> a -> Ordering) -> a -> Array a -> Array a
insertBy cmp x ys =
  let i = maybe 0 (_ + 1) (findLastIndex (\y -> cmp x y == GT) ys)
  in unsafePartial (fromJust (insertAt i x ys))

--------------------------------------------------------------------------------
-- Non-indexed reads -----------------------------------------------------------
--------------------------------------------------------------------------------

-- | Get the first element in an array, or `Nothing` if the array is empty
-- |
-- | Running time: `O(1)`.
head :: forall a. Array a -> Maybe a
head xs = xs !! 0

-- | Get the last element in an array, or `Nothing` if the array is empty
-- |
-- | Running time: `O(1)`.
last :: forall a. Array a -> Maybe a
last xs = xs !! (length xs - 1)

-- | Get all but the first element of an array, creating a new array, or
-- | `Nothing` if the array is empty
-- |
-- | Running time: `O(n)` where `n` is the length of the array
tail :: forall a. Array a -> Maybe (Array a)
tail = uncons' (const Nothing) (\_ xs -> Just xs)

-- | Get all but the last element of an array, creating a new array, or
-- | `Nothing` if the array is empty.
-- |
-- | Running time: `O(n)` where `n` is the length of the array
init :: forall a. Array a -> Maybe (Array a)
init xs
  | null xs = Nothing
  | otherwise = Just (slice zero (length xs - one) xs)

-- | Break an array into its first element and remaining elements.
-- |
-- | Using `uncons` provides a way of writing code that would use cons patterns
-- | in Haskell or pre-PureScript 0.7:
-- | ``` purescript
-- | f (x : xs) = something
-- | f [] = somethingElse
-- | ```
-- | Becomes:
-- | ``` purescript
-- | f arr = case uncons arr of
-- |   Just { head: x, tail: xs } -> something
-- |   Nothing -> somethingElse
-- | ```
uncons :: forall a. Array a -> Maybe { head :: a, tail :: Array a }
uncons = uncons' (const Nothing) \x xs -> Just { head: x, tail: xs }

foreign import uncons'
  :: forall a b
   . (Unit -> b)
  -> (a -> Array a -> b)
  -> Array a
  -> b

-- | Break an array into its last element and all preceding elements.
-- |
-- | Running time: `O(n)` where `n` is the length of the array
unsnoc :: forall a. Array a -> Maybe { init :: Array a, last :: a }
unsnoc xs = { init: _, last: _ } <$> init xs <*> last xs

--------------------------------------------------------------------------------
-- Indexed operations ----------------------------------------------------------
--------------------------------------------------------------------------------

-- | This function provides a safe way to read a value at a particular index
-- | from an array.
index :: forall a. Array a -> Int -> Maybe a
index = indexImpl Just Nothing

foreign import indexImpl
  :: forall a
   . (forall r. r -> Maybe r)
  -> (forall r. Maybe r)
  -> Array a
  -> Int
  -> Maybe a

-- | An infix version of `index`.
infixl 8 index as !!

-- | Find the index of the first element equal to the specified element.
elemIndex :: forall a. Eq a => a -> Array a -> Maybe Int
elemIndex x = findIndex (_ == x)

-- | Find the index of the last element equal to the specified element.
elemLastIndex :: forall a. Eq a => a -> Array a -> Maybe Int
elemLastIndex x = findLastIndex (_ == x)

-- | Find the first index for which a predicate holds.
findIndex :: forall a. (a -> Boolean) -> Array a -> Maybe Int
findIndex = findIndexImpl Just Nothing

foreign import findIndexImpl
  :: forall a
   . (forall b. b -> Maybe b)
  -> (forall b. Maybe b)
  -> (a -> Boolean)
  -> Array a
  -> Maybe Int

-- | Find the last index for which a predicate holds.
findLastIndex :: forall a. (a -> Boolean) -> Array a -> Maybe Int
findLastIndex = findLastIndexImpl Just Nothing

foreign import findLastIndexImpl
  :: forall a
   . (forall b. b -> Maybe b)
  -> (forall b. Maybe b)
  -> (a -> Boolean)
  -> Array a
  -> Maybe Int

-- | Insert an element at the specified index, creating a new array, or
-- | returning `Nothing` if the index is out of bounds.
insertAt :: forall a. Int -> a -> Array a -> Maybe (Array a)
insertAt = _insertAt Just Nothing

foreign import _insertAt
  :: forall a
   . (forall b. b -> Maybe b)
  -> (forall b. Maybe b)
  -> Int
  -> a
  -> Array a
  -> Maybe (Array a)

-- | Delete the element at the specified index, creating a new array, or
-- | returning `Nothing` if the index is out of bounds.
deleteAt :: forall a. Int -> Array a -> Maybe (Array a)
deleteAt = _deleteAt Just Nothing

foreign import _deleteAt
  :: forall a
   . (forall b. b -> Maybe b)
  -> (forall b. Maybe b)
  -> Int
  -> Array a
  -> Maybe (Array a)

-- | Change the element at the specified index, creating a new array, or
-- | returning `Nothing` if the index is out of bounds.
updateAt :: forall a. Int -> a -> Array a -> Maybe (Array a)
updateAt = _updateAt Just Nothing

foreign import _updateAt
  :: forall a
   . (forall b. b -> Maybe b)
  -> (forall b. Maybe b)
  -> Int
  -> a
  -> Array a
  -> Maybe (Array a)

-- | Apply a function to the element at the specified index, creating a new
-- | array, or returning `Nothing` if the index is out of bounds.
modifyAt :: forall a. Int -> (a -> a) -> Array a -> Maybe (Array a)
modifyAt i f xs = maybe Nothing go (xs !! i)
  where
  go x = updateAt i (f x) xs

-- | Update or delete the element at the specified index by applying a
-- | function to the current value, returning a new array or `Nothing` if the
-- | index is out-of-bounds.
alterAt :: forall a. Int -> (a -> Maybe a) -> Array a -> Maybe (Array a)
alterAt i f xs = maybe Nothing go (xs !! i)
  where
  go x = case f x of
    Nothing -> deleteAt i xs
    Just x' -> updateAt i x' xs

--------------------------------------------------------------------------------
-- Transformations -------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Reverse an array, creating a new array.
foreign import reverse :: forall a. Array a -> Array a

-- | Flatten an array of arrays, creating a new array.
foreign import concat :: forall a. Array (Array a) -> Array a

-- | Apply a function to each element in an array, and flatten the results
-- | into a single, new array.
concatMap :: forall a b. (a -> Array b) -> Array a -> Array b
concatMap = flip bind

-- | Filter an array, keeping the elements which satisfy a predicate function,
-- | creating a new array.
foreign import filter :: forall a. (a -> Boolean) -> Array a -> Array a

-- | Partition an array using a predicate function, creating a set of
-- | new arrays. One for the values satisfying the predicate function
-- | and one for values that don't.
foreign import partition
  :: forall a
   . (a -> Boolean)
  -> Array a
  -> { yes :: Array a, no :: Array a }

-- | Filter where the predicate returns a `Boolean` in some `Applicative`.
-- |
-- | ```purescript
-- | powerSet :: forall a. Array a -> Array (Array a)
-- | powerSet = filterA (const [true, false])
-- | ```
filterA :: forall a f. Applicative f => (a -> f Boolean) -> Array a -> f (Array a)
filterA p =
  traverse (\x -> Tuple x <$> p x)
  >>> map (mapMaybe (\(Tuple x b) -> if b then Just x else Nothing))

-- | Apply a function to each element in an array, keeping only the results
-- | which contain a value, creating a new array.
mapMaybe :: forall a b. (a -> Maybe b) -> Array a -> Array b
mapMaybe f = concatMap (maybe [] singleton <<< f)

-- | Filter an array of optional values, keeping only the elements which contain
-- | a value, creating a new array.
catMaybes :: forall a. Array (Maybe a) -> Array a
catMaybes = mapMaybe id

-- | Apply a function to each element in an array, supplying a generated
-- | zero-based index integer along with the element, creating an array
-- | with the new elements.
mapWithIndex :: forall a b. (Int -> a -> b) -> Array a -> Array b
mapWithIndex f xs =
  zipWith f (range 0 (length xs - 1)) xs

-- | Change the elements at the specified indices in index/value pairs.
-- | Out-of-bounds indices will have no effect.
updateAtIndices :: forall t a. Foldable t => t (Tuple Int a) -> Array a -> Array a
updateAtIndices us xs =
  pureST (withArray (\res -> traverse_ (uncurry $ pokeSTArray res) us) xs)

-- | Apply a function to the element at the specified indices,
-- | creating a new array. Out-of-bounds indices will have no effect.
modifyAtIndices :: forall t a. Foldable t => t Int -> (a -> a) -> Array a -> Array a
modifyAtIndices is f xs =
  pureST (withArray (\res -> traverse_ (\i -> modifySTArray res i f) is) xs)

--------------------------------------------------------------------------------
-- Sorting ---------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Sort the elements of an array in increasing order, creating a new array.
sort :: forall a. Ord a => Array a -> Array a
sort xs = sortBy compare xs

-- | Sort the elements of an array in increasing order, where elements are
-- | compared using the specified partial ordering, creating a new array.
sortBy :: forall a. (a -> a -> Ordering) -> Array a -> Array a
sortBy comp xs = sortImpl comp' xs
  where
  comp' x y = case comp x y of
    GT -> 1
    EQ -> 0
    LT -> -1

-- | Sort the elements of an array in increasing order, where elements are
-- | sorted based on a projection
sortWith :: forall a b. Ord b => (a -> b) -> Array a -> Array a
sortWith f = sortBy (comparing f)

foreign import sortImpl :: forall a. (a -> a -> Int) -> Array a -> Array a

--------------------------------------------------------------------------------
-- Subarrays -------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Extract a subarray by a start and end index.
foreign import slice :: forall a. Int -> Int -> Array a -> Array a

-- | Keep only a number of elements from the start of an array, creating a new
-- | array.
foreign import take :: forall a. Int -> Array a -> Array a

-- | Keep only a number of elements from the end of an array, creating a new
-- | array.
takeEnd :: forall a. Int -> Array a -> Array a
takeEnd n xs = drop (length xs - n) xs

-- | Calculate the longest initial subarray for which all element satisfy the
-- | specified predicate, creating a new array.
takeWhile :: forall a. (a -> Boolean) -> Array a -> Array a
takeWhile p xs = (span p xs).init

-- | Drop a number of elements from the start of an array, creating a new array.
foreign import drop :: forall a. Int -> Array a -> Array a

-- | Drop a number of elements from the start of an array, creating a new array.
dropEnd :: forall a. Int -> Array a -> Array a
dropEnd n xs = take (length xs - n) xs

-- | Remove the longest initial subarray for which all element satisfy the
-- | specified predicate, creating a new array.
dropWhile :: forall a. (a -> Boolean) -> Array a -> Array a
dropWhile p xs = (span p xs).rest

-- | Split an array into two parts:
-- |
-- | 1. the longest initial subarray for which all elements satisfy the
-- |    specified predicate
-- | 2. the remaining elements
-- |
-- | ```purescript
-- | span (\n -> n % 2 == 1) [1,3,2,4,5] == { init: [1,3], rest: [2,4,5] }
-- | ```
-- |
-- | Running time: `O(n)`.
span
  :: forall a
   . (a -> Boolean)
  -> Array a
  -> { init :: Array a, rest :: Array a }
span p arr =
  case breakIndex of
    Just 0 ->
      { init: [], rest: arr }
    Just i ->
      { init: slice 0 i arr, rest: slice i (length arr) arr }
    Nothing ->
      { init: arr, rest: [] }
  where
  breakIndex = go 0
  go i =
    -- This looks like a good opportunity to use the Monad Maybe instance,
    -- but it's important to write out an explicit case expression here in
    -- order to ensure that TCO is triggered.
    case index arr i of
      Just x -> if p x then go (i + 1) else Just i
      Nothing -> Nothing

-- | Group equal, consecutive elements of an array into arrays.
-- |
-- | ```purescript
-- | group [1,1,2,2,1] == [[1,1],[2,2],[1]]
-- | ```
group :: forall a. Eq a => Array a -> Array (NonEmpty Array a)
group xs = groupBy eq xs

-- | Sort and then group the elements of an array into arrays.
-- |
-- | ```purescript
-- | group' [1,1,2,2,1] == [[1,1,1],[2,2]]
-- | ```
group' :: forall a. Ord a => Array a -> Array (NonEmpty Array a)
group' = group <<< sort

-- | Group equal, consecutive elements of an array into arrays, using the
-- | specified equivalence relation to detemine equality.
groupBy :: forall a. (a -> a -> Boolean) -> Array a -> Array (NonEmpty Array a)
groupBy op xs =
  pureST do
    result <- emptySTArray
    iter <- iterator (xs !! _)
    iterate iter \x -> void do
      sub <- emptySTArray
      pushWhile (op x) iter sub
      sub_ <- unsafeFreeze sub
      pushSTArray result (x :| sub_)
    unsafeFreeze result

-- | Remove the duplicates from an array, creating a new array.
nub :: forall a. Eq a => Array a -> Array a
nub = nubBy eq

-- | Remove the duplicates from an array, where element equality is determined
-- | by the specified equivalence relation, creating a new array.
nubBy :: forall a. (a -> a -> Boolean) -> Array a -> Array a
nubBy eq xs =
  case uncons xs of
    Just o -> o.head : nubBy eq (filter (\y -> not (o.head `eq` y)) o.tail)
    Nothing -> []

-- | Calculate the union of two arrays. Note that duplicates in the first array
-- | are preserved while duplicates in the second array are removed.
-- |
-- | Running time: `O(n^2)`
union :: forall a. Eq a => Array a -> Array a -> Array a
union = unionBy (==)

-- | Calculate the union of two arrays, using the specified function to
-- | determine equality of elements. Note that duplicates in the first array
-- | are preserved while duplicates in the second array are removed.
unionBy :: forall a. (a -> a -> Boolean) -> Array a -> Array a -> Array a
unionBy eq xs ys = xs <> foldl (flip (deleteBy eq)) (nubBy eq ys) xs

-- | Delete the first element of an array which is equal to the specified value,
-- | creating a new array.
-- |
-- | Running time: `O(n)`
delete :: forall a. Eq a => a -> Array a -> Array a
delete = deleteBy eq

-- | Delete the first element of an array which matches the specified value,
-- | under the equivalence relation provided in the first argument, creating a
-- | new array.
deleteBy :: forall a. (a -> a -> Boolean) -> a -> Array a -> Array a
deleteBy _  _ [] = []
deleteBy eq x ys = maybe ys (\i -> unsafePartial $ fromJust (deleteAt i ys)) (findIndex (eq x) ys)

-- | Delete the first occurrence of each element in the second array from the
-- | first array, creating a new array.
-- |
-- | Running time: `O(n*m)`, where n is the length of the first array, and m is
-- | the length of the second.
difference :: forall a. Eq a => Array a -> Array a -> Array a
difference = foldr delete

infix 5 difference as \\

-- | Calculate the intersection of two arrays, creating a new array. Note that
-- | duplicates in the first array are preserved while duplicates in the second
-- | array are removed.
intersect :: forall a. Eq a => Array a -> Array a -> Array a
intersect = intersectBy eq

-- | Calculate the intersection of two arrays, using the specified equivalence
-- | relation to compare elements, creating a new array. Note that duplicates
-- | in the first array are preserved while duplicates in the second array are
-- | removed.
intersectBy :: forall a. (a -> a -> Boolean) -> Array a -> Array a -> Array a
intersectBy eq xs ys = filter (\x -> isJust (findIndex (eq x) ys)) xs

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
  :: forall a b c
   . (a -> b -> c)
  -> Array a
  -> Array b
  -> Array c

-- | A generalization of `zipWith` which accumulates results in some
-- | `Applicative` functor.
zipWithA
  :: forall m a b c
   . Applicative m
  => (a -> b -> m c)
  -> Array a
  -> Array b
  -> m (Array c)
zipWithA f xs ys = sequence (zipWith f xs ys)

-- | Takes two arrays and returns an array of corresponding pairs.
-- | If one input array is short, excess elements of the longer array are
-- | discarded.
zip :: forall a b. Array a -> Array b -> Array (Tuple a b)
zip = zipWith Tuple

-- | Transforms an array of pairs into an array of first components and an
-- | array of second components.
unzip :: forall a b. Array (Tuple a b) -> Tuple (Array a) (Array b)
unzip xs =
  pureST do
    fsts <- emptySTArray
    snds <- emptySTArray
    iter <- iterator (xs !! _)
    iterate iter \(Tuple fst snd) -> do
      void $ pushSTArray fsts fst
      void $ pushSTArray snds snd
    fsts' <- unsafeFreeze fsts
    snds' <- unsafeFreeze snds
    pure $ Tuple fsts' snds'

-- | Perform a fold using a monadic step function.
foldM :: forall m a b. Monad m => (a -> b -> m a) -> a -> Array b -> m a
foldM f a = uncons' (\_ -> pure a) (\b bs -> f a b >>= \a' -> foldM f a' bs)

foldRecM :: forall m a b. MonadRec m => (a -> b -> m a) -> a -> Array b -> m a
foldRecM f a array = tailRecM2 go a 0
  where
  go res i
    | i >= length array = pure (Done res)
    | otherwise = do
        res' <- f res (unsafePartial (unsafeIndex array i))
        pure (Loop { a: res', b: i + 1 })

-- | Find the element of an array at the specified index.
unsafeIndex :: forall a. Partial => Array a -> Int -> a
unsafeIndex = unsafeIndexImpl

foreign import unsafeIndexImpl :: forall a. Array a -> Int -> a
