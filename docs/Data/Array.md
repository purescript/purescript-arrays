## Module Data.Array

Helper functions for working with immutable Javascript arrays.

_Note_: Depending on your use-case, you may prefer to use `Data.List` or
`Data.Sequence` instead, which might give better performance for certain
use cases. This module is useful when integrating with JavaScript libraries
which use arrays, but immutable arrays are not a practical data structure
for many use cases due to their poor asymptotics.

In addition to the functions in this module, Arrays have a number of
useful instances:

* `Functor`, which provides `map :: forall a b. (a -> b) -> Array a ->
  Array b`
* `Apply`, which provides `(<*>) :: forall a b. Array (a -> b) -> Array a
  -> Array b`. This function works a bit like a Cartesian product; the
  result array is constructed by applying each function in the first
  array to each value in the second, so that the result array ends up with
  a length equal to the product of the two arguments' lengths.
* `Bind`, which provides `(>>=) :: forall a b. (a -> Array b) -> Array a
  -> Array b` (this is the same as `concatMap`).
* `Semigroup`, which provides `(<>) :: forall a. Array a -> Array a ->
  Array a`, for concatenating arrays.
* `Foldable`, which provides a slew of functions for *folding* (also known
  as *reducing*) arrays down to one value. For example,
  `Data.Foldable.or` tests whether an array of `Boolean` values contains
  at least one `true` value.
* `Traversable`, which provides the PureScript version of a for-loop,
  allowing you to iterate over an array and accumulate effects.


#### `singleton`

``` purescript
singleton :: forall a. a -> Array a
```

Create an array of one element

#### `range`

``` purescript
range :: Int -> Int -> Array Int
```

Create an array containing a range of integers, including both endpoints.

#### `(..)`

``` purescript
(..) :: Int -> Int -> Array Int
```

_non-associative / precedence 8_

An infix synonym for `range`.

#### `replicate`

``` purescript
replicate :: forall a. Int -> a -> Array a
```

Create an array with repeated instances of a value.

#### `replicateM`

``` purescript
replicateM :: forall m a. (Monad m) => Int -> m a -> m (Array a)
```

Perform a monadic action `n` times collecting all of the results.

#### `some`

``` purescript
some :: forall f a. (Alternative f, Lazy (f (Array a))) => f a -> f (Array a)
```

Attempt a computation multiple times, requiring at least one success.

The `Lazy` constraint is used to generate the result lazily, to ensure
termination.

#### `many`

``` purescript
many :: forall f a. (Alternative f, Lazy (f (Array a))) => f a -> f (Array a)
```

Attempt a computation multiple times, returning as many successful results
as possible (possibly zero).

The `Lazy` constraint is used to generate the result lazily, to ensure
termination.

#### `null`

``` purescript
null :: forall a. Array a -> Boolean
```

#### `length`

``` purescript
length :: forall a. Array a -> Int
```

Get the number of elements in an array.

#### `cons`

``` purescript
cons :: forall a. a -> Array a -> Array a
```

#### `(:)`

``` purescript
(:) :: forall a. a -> Array a -> Array a
```

_right-associative / precedence 6_

An infix alias for `cons`.

Note, the running time of this function is `O(n)`.

#### `snoc`

``` purescript
snoc :: forall a. Array a -> a -> Array a
```

Append an element to the end of an array, creating a new array.

#### `insert`

``` purescript
insert :: forall a. (Ord a) => a -> Array a -> Array a
```

Insert an element into a sorted array.

#### `insertBy`

``` purescript
insertBy :: forall a. (a -> a -> Ordering) -> a -> Array a -> Array a
```

Insert an element into a sorted array, using the specified function to
determine the ordering of elements.

#### `head`

``` purescript
head :: forall a. Array a -> Maybe a
```

#### `last`

``` purescript
last :: forall a. Array a -> Maybe a
```

Get the last element in an array, or `Nothing` if the array is empty

Running time: `O(1)`.

#### `tail`

``` purescript
tail :: forall a. Array a -> Maybe (Array a)
```

Get all but the first element of an array, creating a new array, or `Nothing` if the array is empty

Running time: `O(n)` where `n` is the length of the array

#### `init`

``` purescript
init :: forall a. Array a -> Maybe (Array a)
```

Get all but the last element of an array, creating a new array, or `Nothing` if the array is empty.

Running time: `O(n)` where `n` is the length of the array

#### `uncons`

``` purescript
uncons :: forall a. Array a -> Maybe { head :: a, tail :: Array a }
```

Break an array into its first element and remaining elements.

Using `uncons` provides a way of writing code that would use cons patterns
in Haskell or pre-PureScript 0.7:
``` purescript
f (x : xs) = something
f [] = somethingElse
```
Becomes:
``` purescript
f arr = case uncons arr of
  Just { head: x, tail: xs } -> something
  Nothing -> somethingElse
```

#### `index`

``` purescript
index :: forall a. Array a -> Int -> Maybe a
```

#### `(!!)`

``` purescript
(!!) :: forall a. Array a -> Int -> Maybe a
```

_left-associative / precedence 8_

An infix version of `index`.

#### `elemIndex`

``` purescript
elemIndex :: forall a. (Eq a) => a -> Array a -> Maybe Int
```

Find the index of the first element equal to the specified element.

#### `elemLastIndex`

``` purescript
elemLastIndex :: forall a. (Eq a) => a -> Array a -> Maybe Int
```

Find the index of the last element equal to the specified element.

#### `findIndex`

``` purescript
findIndex :: forall a. (a -> Boolean) -> Array a -> Maybe Int
```

Find the first index for which a predicate holds.

#### `findLastIndex`

``` purescript
findLastIndex :: forall a. (a -> Boolean) -> Array a -> Maybe Int
```

Find the last index for which a predicate holds.

#### `insertAt`

``` purescript
insertAt :: forall a. Int -> a -> Array a -> Maybe (Array a)
```

Insert an element at the specified index, creating a new array, or
returning `Nothing` if the index is out of bounds.

#### `deleteAt`

``` purescript
deleteAt :: forall a. Int -> Array a -> Maybe (Array a)
```

Delete the element at the specified index, creating a new array, or
returning `Nothing` if the index is out of bounds.

#### `updateAt`

``` purescript
updateAt :: forall a. Int -> a -> Array a -> Maybe (Array a)
```

Change the element at the specified index, creating a new array, or
returning `Nothing` if the index is out of bounds.

#### `modifyAt`

``` purescript
modifyAt :: forall a. Int -> (a -> a) -> Array a -> Maybe (Array a)
```

Apply a function to the element at the specified index, creating a new
array, or returning `Nothing` if the index is out of bounds.

#### `alterAt`

``` purescript
alterAt :: forall a. Int -> (a -> Maybe a) -> Array a -> Maybe (Array a)
```

Update or delete the element at the specified index by applying a
function to the current value, returning a new array or `Nothing` if the
index is out-of-bounds.

#### `reverse`

``` purescript
reverse :: forall a. Array a -> Array a
```

#### `concat`

``` purescript
concat :: forall a. Array (Array a) -> Array a
```

Flatten an array of arrays, creating a new array.

#### `concatMap`

``` purescript
concatMap :: forall a b. (a -> Array b) -> Array a -> Array b
```

Apply a function to each element in an array, and flatten the results
into a single, new array.

#### `filter`

``` purescript
filter :: forall a. (a -> Boolean) -> Array a -> Array a
```

Filter an array, keeping the elements which satisfy a predicate function,
creating a new array.

#### `filterM`

``` purescript
filterM :: forall a m. (Monad m) => (a -> m Boolean) -> Array a -> m (Array a)
```

Filter where the predicate returns a monadic `Boolean`.

```purescript
powerSet :: forall a. [a] -> [[a]]
powerSet = filterM (const [true, false])
```

#### `mapMaybe`

``` purescript
mapMaybe :: forall a b. (a -> Maybe b) -> Array a -> Array b
```

Apply a function to each element in an array, keeping only the results
which contain a value, creating a new array.

#### `catMaybes`

``` purescript
catMaybes :: forall a. Array (Maybe a) -> Array a
```

Filter an array of optional values, keeping only the elements which contain
a value, creating a new array.

#### `sort`

``` purescript
sort :: forall a. (Ord a) => Array a -> Array a
```

#### `sortBy`

``` purescript
sortBy :: forall a. (a -> a -> Ordering) -> Array a -> Array a
```

Sort the elements of an array in increasing order, where elements are compared using
the specified partial ordering, creating a new array.

#### `slice`

``` purescript
slice :: forall a. Int -> Int -> Array a -> Array a
```

#### `take`

``` purescript
take :: forall a. Int -> Array a -> Array a
```

Keep only a number of elements from the start of an array, creating a new
array.

#### `takeWhile`

``` purescript
takeWhile :: forall a. (a -> Boolean) -> Array a -> Array a
```

Calculate the longest initial subarray for which all element satisfy the
specified predicate, creating a new array.

#### `drop`

``` purescript
drop :: forall a. Int -> Array a -> Array a
```

Drop a number of elements from the start of an array, creating a new array.

#### `dropWhile`

``` purescript
dropWhile :: forall a. (a -> Boolean) -> Array a -> Array a
```

Remove the longest initial subarray for which all element satisfy the
specified predicate, creating a new array.

#### `span`

``` purescript
span :: forall a. (a -> Boolean) -> Array a -> { init :: Array a, rest :: Array a }
```

Split an array into two parts:

1. the longest initial subarray for which all element satisfy the specified
   predicate
2. the remaining elements

```purescript
span (\n -> n % 2 == 1) [1,3,2,4,5] == { init: [1,3], rest: [2,4,5] }
```

#### `group`

``` purescript
group :: forall a. (Eq a) => Array a -> Array (Array a)
```

Group equal, consecutive elements of an array into arrays.

```purescript
group [1,1,2,2,1] == [[1,1],[2,2],[1]]
```

#### `group'`

``` purescript
group' :: forall a. (Ord a) => Array a -> Array (Array a)
```

Sort and then group the elements of an array into arrays.

```purescript
group' [1,1,2,2,1] == [[1,1,1],[2,2]]
```

#### `groupBy`

``` purescript
groupBy :: forall a. (a -> a -> Boolean) -> Array a -> Array (Array a)
```

Group equal, consecutive elements of an array into arrays, using the
specified equivalence relation to detemine equality.

#### `nub`

``` purescript
nub :: forall a. (Eq a) => Array a -> Array a
```

Remove the duplicates from an array, creating a new array.

#### `nubBy`

``` purescript
nubBy :: forall a. (a -> a -> Boolean) -> Array a -> Array a
```

Remove the duplicates from an array, where element equality is determined
by the specified equivalence relation, creating a new array.

#### `union`

``` purescript
union :: forall a. (Eq a) => Array a -> Array a -> Array a
```

Calculate the union of two lists.

Running time: `O(n^2)`

#### `unionBy`

``` purescript
unionBy :: forall a. (a -> a -> Boolean) -> Array a -> Array a -> Array a
```

Calculate the union of two arrays, using the specified function to
determine equality of elements.

#### `delete`

``` purescript
delete :: forall a. (Eq a) => a -> Array a -> Array a
```

Delete the first element of an array which is equal to the specified value,
creating a new array.

#### `deleteBy`

``` purescript
deleteBy :: forall a. (a -> a -> Boolean) -> a -> Array a -> Array a
```

Delete the first element of an array which matches the specified value,
under the equivalence relation provided in the first argument, creating a
new array.

#### `(\\)`

``` purescript
(\\) :: forall a. (Eq a) => Array a -> Array a -> Array a
```

_non-associative / precedence 5_

Delete the first occurrence of each element in the second array from the
first array, creating a new array.

#### `intersect`

``` purescript
intersect :: forall a. (Eq a) => Array a -> Array a -> Array a
```

Calculate the intersection of two arrays, creating a new array.

#### `intersectBy`

``` purescript
intersectBy :: forall a. (a -> a -> Boolean) -> Array a -> Array a -> Array a
```

Calculate the intersection of two arrays, using the specified equivalence
relation to compare elements, creating a new array.

#### `zipWith`

``` purescript
zipWith :: forall a b c. (a -> b -> c) -> Array a -> Array b -> Array c
```

Apply a function to pairs of elements at the same index in two arrays,
collecting the results in a new array.

If one array is longer, elements will be discarded from the longer array.

For example

```purescript
zipWith (*) [1, 2, 3] [4, 5, 6, 7] == [4, 10, 18]
```

#### `zipWithA`

``` purescript
zipWithA :: forall m a b c. (Applicative m) => (a -> b -> m c) -> Array a -> Array b -> m (Array c)
```

A generalization of `zipWith` which accumulates results in some `Applicative`
functor.

#### `zip`

``` purescript
zip :: forall a b. Array a -> Array b -> Array (Tuple a b)
```

Rakes two lists and returns a list of corresponding pairs.
If one input list is short, excess elements of the longer list are discarded.

#### `unzip`

``` purescript
unzip :: forall a b. Array (Tuple a b) -> Tuple (Array a) (Array b)
```

Transforms a list of pairs into a list of first components and a list of
second components.

#### `foldM`

``` purescript
foldM :: forall m a b. (Monad m) => (a -> b -> m a) -> a -> Array b -> m a
```

Perform a fold using a monadic step function.


