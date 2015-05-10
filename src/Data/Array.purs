-- | Helper functions for working with immutable Javascript arrays.
-- |
-- | _Note_: Depending on your use-case, you may prefer to use `Data.List` or
-- | `Data.Sequence` instead, which might give better performance for certain
-- | use cases. This module is useful when integrating with JavaScript libraries
-- | which use arrays, but immutable arrays are not a practical data structure
-- | for many use cases due to their poor asymptotics.
module Data.Array
  ( singleton
  , (..), range
  , replicate
  , replicateM
  , some
  , many

  , null
  , length

  , (:), cons
  , snoc

  , head
  , last
  , tail
  , init
  , uncons

  , (!!), index
  , elemIndex
  , elemLastIndex
  , findIndex
  , findLastIndex
  , insertAt
  , deleteAt
  , updateAt
  , modifyAt

  , reverse
  , concat
  , concatMap
  , filter
  , filterM
  , mapMaybe
  , catMaybes

  , sort
  , sortBy

  , take
  , takeWhile
  , drop
  , dropWhile
  , span
  , group
  , group'
  , groupBy

  , nub
  , nubBy
  , delete
  , deleteBy

  , (\\)
  , intersect
  , intersectBy

  , zipWith
  , zipWithA

  , foldM
  ) where

import Control.Alt (Alt, (<|>))
import Control.Alternative (Alternative)
import Control.MonadPlus (MonadPlus)
import Control.Plus (Plus)
import Control.Lazy (Lazy, defer)
import Data.Foldable (Foldable, foldr)
import Data.Functor.Invariant (Invariant, imapF)
import Data.Maybe (Maybe(..), maybe, isJust)
import Data.Monoid (Monoid, mempty)
import Data.Traversable (Traversable, traverse, sequence)

--------------------------------------------------------------------------------
-- Array creation --------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Create an array of one element
singleton :: forall a. a -> Array a
singleton a = [a]

-- | Create an array containing a range of integers, including both endpoints.
foreign import range :: Int -> Int -> Array Int

infix 8 ..

-- | An infix synonym for `range`.
(..) :: Int -> Int -> Array Int
(..) = range

-- | Create an array with repeated instances of a value.
foreign import replicate :: forall a. Int -> a -> Array a

-- | Perform a monadic action `n` times collecting all of the results.
replicateM :: forall m a. (Monad m) => Int -> m a -> m (Array a)
replicateM n m | n < 1 = return []
               | otherwise = do a <- m
                                as <- replicateM (n - 1) m
                                return (a : as)

-- | Attempt a computation multiple times, requiring at least one success.
-- |
-- | The `Lazy` constraint is used to generate the result lazily, to ensure
-- | termination.
some :: forall f a. (Alternative f, Lazy (f (Array a))) => f a -> f (Array a)
some v = (:) <$> v <*> defer (\_ -> many v)

-- | Attempt a computation multiple times, returning as many successful results
-- | as possible (possibly zero).
-- |
-- | The `Lazy` constraint is used to generate the result lazily, to ensure
-- | termination.
many :: forall f a. (Alternative f, Lazy (f (Array a))) => f a -> f (Array a)
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

infixr 6 :

-- | An infix alias for `cons`.
-- |
-- | Note, the running time of this function is `O(n)`.
(:) :: forall a. a -> Array a -> Array a
(:) = cons

-- | Append an element to the end of an array, creating a new array.
foreign import snoc :: forall a. Array a -> a -> Array a

--------------------------------------------------------------------------------
-- Non-indexed reads -----------------------------------------------------------
--------------------------------------------------------------------------------

-- | Get the first element in an array, or `Nothing` if the array is empty
-- |
-- | Running time: `O(1)`.
head :: forall a. Array a -> Maybe a
head = uncons' (const Nothing) (\x _ -> Just x)

-- | Get the last element in an array, or `Nothing` if the array is empty
-- |
-- | Running time: `O(1)`.
last :: forall a. Array a -> Maybe a
last xs = xs !! (length xs - 1)

-- | Get all but the first element of an array, creating a new array, or `Nothing` if the array is empty
-- |
-- | Running time: `O(n)` where `n` is the length of the array
tail :: forall a. Array a -> Maybe (Array a)
tail = uncons' (const Nothing) (\_ xs -> Just xs)

-- | Get all but the last element of an array, creating a new array, or `Nothing` if the array is empty.
-- |
-- | Running time: `O(n)` where `n` is the length of the array
init :: forall a. Array a -> Maybe (Array a)
init xs | null xs = Nothing
        | otherwise = Just (slice zero (length xs - one) xs)

-- | Break an array into its first element, and the remaining elements
uncons :: forall a. Array a -> Maybe { head :: a, tail :: Array a }
uncons = uncons' (const Nothing) \x xs -> Just { head: x, tail: xs }

foreign import uncons' :: forall a b. (Unit -> b)
                                   -> (a -> Array a -> b)
                                   -> Array a
                                   -> b

--------------------------------------------------------------------------------
-- Indexed operations ----------------------------------------------------------
--------------------------------------------------------------------------------

-- | This function provides a safe way to read a value at a particular index
-- | from an array.
index :: forall a. Array a -> Int -> Maybe a
index = indexImpl Just Nothing

foreign import indexImpl :: forall a. (forall r. r -> Maybe r)
                                   -> (forall r. Maybe r)
                                   -> Array a
                                   -> Int
                                   -> Maybe a

infixl 8 !!

-- | An infix version of `index`.
(!!) :: forall a. Array a -> Int -> Maybe a
(!!) = index

-- | Find the index of the first element equal to the specified element.
elemIndex :: forall a. (Eq a) => a -> Array a -> Maybe Int
elemIndex x = findIndex (== x)

-- | Find the index of the last element equal to the specified element.
elemLastIndex :: forall a. (Eq a) => a -> Array a -> Maybe Int
elemLastIndex x = findLastIndex (== x)

-- | Find the first index for which a predicate holds.
findIndex :: forall a. (a -> Boolean) -> Array a -> Maybe Int
findIndex = findIndexImpl Just Nothing

foreign import findIndexImpl :: forall a. (forall b. b -> Maybe b)
                                       -> (forall b. Maybe b)
                                       -> (a -> Boolean)
                                       -> (Array a)
                                       -> (Maybe Int)

-- | Find the last index for which a predicate holds.
findLastIndex :: forall a. (a -> Boolean) -> Array a -> Maybe Int
findLastIndex = findLastIndexImpl Just Nothing

foreign import findLastIndexImpl :: forall a. (forall b. b -> Maybe b)
                                           -> (forall b. Maybe b)
                                           -> (a -> Boolean)
                                           -> (Array a)
                                           -> (Maybe Int)

-- | Insert an element at the specified index, creating a new array.
foreign import insertAt :: forall a. Int -> a -> Array a -> Array a

-- | Delete the element at the specified index, creating a new array.
foreign import deleteAt :: forall a. Int -> Int -> Array a -> Array a

-- | Change the element at the specified index, creating a new array.
foreign import updateAt :: forall a. Int -> a -> Array a -> Array a

-- | Apply a function to the element at the specified index, creating a new array.
modifyAt :: forall a. Int -> (a -> a) -> Array a -> Array a
modifyAt i f xs = maybe xs (\x -> updateAt i (f x) xs) (xs !! i)

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

-- | Filter where the predicate returns a monadic `Boolean`.
-- |
-- | ```purescript
-- | powerSet :: forall a. [a] -> [[a]]
-- | powerSet = filterM (const [true, false])
-- | ```
filterM :: forall a m. (Monad m) => (a -> m Boolean) -> Array a -> m (Array a)
filterM p = uncons' (\_ -> pure []) \x xs -> do
    b <- p x
    xs' <- filterM p xs
    return if b
           then x : xs'
           else xs'

-- | Apply a function to each element in an array, keeping only the results
-- | which contain a value, creating a new array.
mapMaybe :: forall a b. (a -> Maybe b) -> Array a -> Array b
mapMaybe f = concatMap (maybe [] singleton <<< f)

-- | Filter an array of optional values, keeping only the elements which contain
-- | a value, creating a new array.
catMaybes :: forall a. Array (Maybe a) -> Array a
catMaybes = mapMaybe id

--------------------------------------------------------------------------------
-- Sorting ---------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Sort the elements of an array in increasing order, creating a new array.
sort :: forall a. (Ord a) => Array a -> Array a
sort xs = sortBy compare xs

-- | Sort the elements of an array in increasing order, where elements are compared using
-- | the specified partial ordering, creating a new array.
sortBy :: forall a. (a -> a -> Ordering) -> Array a -> Array a
sortBy comp xs = sortImpl comp' xs
  where
  comp' x y = case comp x y of
    GT -> 1
    EQ -> 0
    LT -> -1

foreign import sortImpl :: forall a. (a -> a -> Int) -> Array a -> Array a

--------------------------------------------------------------------------------
-- Subarrays -------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Extract a subarray by a start index and length.
foreign import slice :: forall a. Int -> Int -> Array a -> Array a

-- | Keep only a number of elements from the start of an array, creating a new
-- | array.
take :: forall a. Int -> Array a -> Array a
take = slice 0

-- | Calculate the longest initial subarray for which all element satisfy the
-- | specified predicate, creating a new array.
takeWhile :: forall a. (a -> Boolean) -> Array a -> Array a
takeWhile p xs = (span p xs).init

-- | Drop a number of elements from the start of an array, creating a new array.
foreign import drop :: forall a. Int -> Array a -> Array a

-- | Remove the longest initial subarray for which all element satisfy the
-- | specified predicate, creating a new array.
dropWhile :: forall a. (a -> Boolean) -> Array a -> Array a
dropWhile p xs = (span p xs).rest

-- | Split an array into two parts:
-- |
-- | 1. the longest initial subarray for which all element satisfy the specified
-- |    predicate
-- | 2. the remaining elements
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

-- | Group equal, consecutive elements of an array into arrays.
-- |
-- | ```purescript
-- | group [1,1,2,2,1] == [[1,1],[2,2],[1]]
-- | ```
group :: forall a. (Eq a) => Array a -> Array (Array a)
group xs = groupBy eq xs

-- | Sort and then group the elements of an array into arrays.
-- |
-- | ```purescript
-- | group' [1,1,2,2,1] == [[1,1,1],[2,2]]
-- | ```
group' :: forall a. (Ord a) => Array a -> Array (Array a)
group' = group <<< sort

-- | Group equal, consecutive elements of an array into arrays, using the
-- | specified equivalence relation to detemine equality.
groupBy :: forall a. (a -> a -> Boolean) -> Array a -> Array (Array a)
groupBy op = go []
  where
  go :: Array (Array a) -> Array a -> Array (Array a)
  go acc []     = reverse acc
  go acc xs = case uncons xs of
                Just o -> let sp = span (op o.head) o.tail
                          in go ((o.head : sp.init) : acc) sp.rest

--------------------------------------------------------------------------------
-- Set-like operations ---------------------------------------------------------
--------------------------------------------------------------------------------

-- | Remove the duplicates from an array, creating a new array.
nub :: forall a. (Eq a) => Array a -> Array a
nub = nubBy eq

-- | Remove the duplicates from an array, where element equality is determined
-- | by the specified equivalence relation, creating a new array.
nubBy :: forall a. (a -> a -> Boolean) -> Array a -> Array a
nubBy _ [] = []
nubBy eq xs = case uncons xs of
                Just o -> o.head : nubBy eq (filter (\y -> not (o.head `eq` y)) o.tail)

-- | Delete the first element of an array which is equal to the specified value,
-- | creating a new array.
delete :: forall a. (Eq a) => a -> Array a -> Array a
delete = deleteBy eq

-- | Delete the first element of an array which matches the specified value,
-- | under the equivalence relation provided in the first argument, creating a
-- | new array.
deleteBy :: forall a. (a -> a -> Boolean) -> a -> Array a -> Array a
deleteBy _  _ [] = []
deleteBy eq x ys = maybe ys (\i -> deleteAt i one ys) (findIndex (eq x) ys)

infix 5 \\

-- | Delete the first occurrence of each element in the second array from the
-- | first array, creating a new array.
(\\) :: forall a. (Eq a) => Array a -> Array a -> Array a
(\\) xs ys | null xs = []
           | otherwise = uncons' (const xs) (\y ys -> delete y xs \\ ys) ys

-- | Calculate the intersection of two arrays, creating a new array.
intersect :: forall a. (Eq a) => Array a -> Array a -> Array a
intersect = intersectBy eq

-- | Calculate the intersection of two arrays, using the specified equivalence
-- | relation to compare elements, creating a new array.
intersectBy :: forall a. (a -> a -> Boolean) -> Array a -> Array a -> Array a
intersectBy eq xs ys = filter (\x -> isJust (findIndex (eq x) ys)) xs

--------------------------------------------------------------------------------
-- Zipping ---------------------------------------------------------------------
--------------------------------------------------------------------------------

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
foreign import zipWith :: forall a b c. (a -> b -> c) -> Array a -> Array b -> Array c

-- | A generalization of `zipWith` which accumulates results in some `Applicative`
-- | functor.
zipWithA :: forall m a b c. (Applicative m) => (a -> b -> m c) -> Array a -> Array b -> m (Array c)
zipWithA f xs ys = sequence (zipWith f xs ys)

--------------------------------------------------------------------------------
-- Folding ---------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Perform a fold using a monadic step function.
foldM :: forall m a b. (Monad m) => (a -> b -> m a) -> a -> Array b -> m a
foldM f a = uncons' (\_ -> return a) (\b bs -> f a b >>= \a' -> foldM f a' bs)

foreign import foldrArray :: forall a b. (a -> b -> b) -> b -> Array a -> b

foreign import foldlArray :: forall a b. (b -> a -> b) -> b -> Array a -> b

--------------------------------------------------------------------------------
-- Non-Prelude instances -------------------------------------------------------
--------------------------------------------------------------------------------

instance altArray :: Alt Array where
  alt = append

instance plusArray :: Plus Array where
  empty = []

instance alternativeArray :: Alternative Array

instance monadPlusArray :: MonadPlus Array

instance foldableArray :: Foldable Array where
  foldr f z xs = foldrArray f z xs
  foldl f z xs = foldlArray f z xs
  foldMap f xs = foldr (\x acc -> f x <> acc) mempty xs

instance traversableArray :: Traversable Array where
  traverse f = uncons' (\_ -> pure []) (\x xs -> (:) <$> (f x) <*> traverse f xs)
  sequence = uncons' (\_ -> pure []) (\x xs -> (:) <$> x <*> sequence xs)

instance invariantArray :: Invariant Array where
  imap = imapF
