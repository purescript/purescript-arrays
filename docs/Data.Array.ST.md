# Module Documentation

## Module Data.Array.ST


Helper functions for working with mutable arrays using the `ST` effect.

This module can be used when performance is important and mutation is a local effect.

#### `STArray`

``` purescript
data STArray :: * -> * -> *
```

A reference to a mutable array.

The first type parameter represents the memory region which the array belongs to.
The second type parameter defines the type of elements of the mutable array.

The runtime representation of a value of type `STArray h a` is the same as that of `[a]`,
except that mutation is allowed.

#### `Assoc`

``` purescript
type Assoc a = { index :: Int, value :: a }
```

An element and its index

#### `runSTArray`

``` purescript
runSTArray :: forall a r. (forall h. Eff (st :: ST h | r) (STArray h a)) -> Eff r [a]
```

Freeze a mutable array, creating an immutable array. Use this function as you would use
`runST` to freeze a mutable reference.

The rank-2 type prevents the reference from escaping the scope of `runSTArray`.

#### `emptySTArray`

``` purescript
emptySTArray :: forall a h r. Eff (st :: ST h | r) (STArray h a)
```

Create an empty mutable array.

#### `peekSTArray`

``` purescript
peekSTArray :: forall a h r. STArray h a -> Int -> Eff (st :: ST h | r) (Maybe a)
```

Read the value at the specified index in a mutable array.

#### `pokeSTArray`

``` purescript
pokeSTArray :: forall a h r. STArray h a -> Int -> a -> Eff (st :: ST h | r) Boolean
```

Change the value at the specified index in a mutable array.

#### `pushAllSTArray`

``` purescript
pushAllSTArray :: forall a h r. STArray h a -> [a] -> Eff (st :: ST h | r) Int
```

Append the values in an immutable array to the end of a mutable array.

#### `pushSTArray`

``` purescript
pushSTArray :: forall a h r. STArray h a -> a -> Eff (st :: ST h | r) Int
```

Append an element to the end of a mutable array.

#### `spliceSTArray`

``` purescript
spliceSTArray :: forall a h r. STArray h a -> Int -> Int -> [a] -> Eff (st :: ST h | r) [a]
```

Remove and/or insert elements from/into a mutable array at the specified index.

#### `freeze`

``` purescript
freeze :: forall a h r. STArray h a -> Eff (st :: ST h | r) [a]
```

Create an immutable copy of a mutable array.

#### `thaw`

``` purescript
thaw :: forall a h r. [a] -> Eff (st :: ST h | r) (STArray h a)
```

Create a mutable copy of an immutable array.

#### `toAssocArray`

``` purescript
toAssocArray :: forall a h r. STArray h a -> Eff (st :: ST h | r) [Assoc a]
```

Create an immutable copy of a mutable array, where each element
is labelled with its index in the original array.



