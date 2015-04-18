# Module Documentation

## Module Data.Array.Unsafe


Unsafe helper functions for working with immutable arrays.

_Note_: these functions should be used with care, and may result in unspecified
behavior, including runtime exceptions.

#### `head`

``` purescript
head :: forall a. [a] -> a
```

Get the first element of a non-empty array.

Running time: `O(1)`.

#### `tail`

``` purescript
tail :: forall a. [a] -> [a]
```

Get all but the first element of a non-empty array.

Running time: `O(n)`, where `n` is the length of the array.

#### `last`

``` purescript
last :: forall a. [a] -> a
```

Get the last element of a non-empty array.

Running time: `O(1)`.

#### `init`

``` purescript
init :: forall a. [a] -> [a]
```

Get all but the last element of a non-empty array.

Running time: `O(n)`, where `n` is the length of the array.



