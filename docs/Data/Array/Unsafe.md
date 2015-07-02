## Module Data.Array.Unsafe

Unsafe helper functions for working with immutable arrays.

_Note_: these functions should be used with care, and may result in unspecified
behavior, including runtime exceptions.

#### `unsafeIndex`

``` purescript
unsafeIndex :: forall a. Array a -> Int -> a
```

Find the element of an array at the specified index.

Note: this function can cause unpredictable failure at runtime if the index is out-of-bounds.

#### `head`

``` purescript
head :: forall a. Array a -> a
```

Get the first element of a non-empty array.

Running time: `O(1)`.

#### `tail`

``` purescript
tail :: forall a. Array a -> Array a
```

Get all but the first element of a non-empty array.

Running time: `O(n)`, where `n` is the length of the array.

#### `last`

``` purescript
last :: forall a. Array a -> a
```

Get the last element of a non-empty array.

Running time: `O(1)`.

#### `init`

``` purescript
init :: forall a. Array a -> Array a
```

Get all but the last element of a non-empty array.

Running time: `O(n)`, where `n` is the length of the array.


