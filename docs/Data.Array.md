## Module Data.Array

Helper functions for working with immutable Javascript arrays.

_Note_: Depending on your use-case, you may prefer to use `Data.List` or
`Data.Sequence` instead, which might give better performance for certain
use cases. This module is useful when integrating with JavaScript libraries
which use arrays, but immutable arrays are not a practical data structure
for many use cases due to their poor asymptotics.

#### `singleton`

``` purescript
singleton :: forall a. a -> Array a
```

#### `range`

``` purescript
range :: Int -> Int -> Array Int
```

Create an array containing a range of integers, including both endpoints.

#### `(..)`

``` purescript
(..) :: Int -> Int -> Array Int
```

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

An infix alias for `cons`.

Note, the running time of this function is `O(n)`.

#### `snoc`

``` purescript
snoc :: forall a. Array a -> a -> Array a
```

Append an element to the end of an array, creating a new array.

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

Break an array into its first element, and the remaining elements

#### `index`

``` purescript
index :: forall a. Array a -> Int -> Maybe a
```

#### `(!!)`

``` purescript
(!!) :: forall a. Array a -> Int -> Maybe a
```

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
insertAt :: forall a. Int -> a -> Array a -> Array a
```

Insert an element at the specified index, creating a new array.

#### `deleteAt`

``` purescript
deleteAt :: forall a. Int -> Int -> Array a -> Array a
```

Delete the element at the specified index, creating a new array.

#### `updateAt`

``` purescript
updateAt :: forall a. Int -> a -> Array a -> Array a
```

Change the element at the specified index, creating a new array.

#### `modifyAt`

``` purescript
modifyAt :: forall a. Int -> (a -> a) -> Array a -> Array a
```

Apply a function to the element at the specified index, creating a new array.

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

#### `nubBy`

``` purescript
nubBy :: forall a. (a -> a -> Boolean) -> Array a -> Array a
```

Remove the duplicates from an array, where element equality is determined
by the specified equivalence relation, creating a new array.

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

#### `zipWithA`

``` purescript
zipWithA :: forall m a b c. (Applicative m) => (a -> b -> m c) -> Array a -> Array b -> m (Array c)
```

A generalization of `zipWith` which accumulates results in some `Applicative`
functor.

#### `foldM`

``` purescript
foldM :: forall m a b. (Monad m) => (a -> b -> m a) -> a -> Array b -> m a
```


