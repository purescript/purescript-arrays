-- | Helper functions for working with mutable arrays using the `ST` effect.
-- |
-- | This module can be used when performance is important and mutation is a local effect.

module Data.Array.ST
  ( STArray(..)
  , Assoc()
  , runSTArray
  , emptySTArray
  , peekSTArray
  , pokeSTArray
  , pushSTArray
  , pushAllSTArray
  , spliceSTArray
  , freeze, thaw
  , toAssocArray
  ) where

import Control.Monad.Eff (Eff())
import Control.Monad.ST (ST())
import Data.Function (Fn2(), runFn2, Fn3(), runFn3, Fn4(), runFn4)
import Data.Maybe (Maybe(..))

-- | A reference to a mutable array.
-- |
-- | The first type parameter represents the memory region which the array belongs to.
-- | The second type parameter defines the type of elements of the mutable array.
-- |
-- | The runtime representation of a value of type `STArray h a` is the same as that of `[a]`,
-- | except that mutation is allowed.
foreign import data STArray :: * -> * -> *

-- | An element and its index
type Assoc a = { value :: a, index :: Int }

-- | Freeze a mutable array, creating an immutable array. Use this function as you would use
-- | `runST` to freeze a mutable reference.
-- |
-- | The rank-2 type prevents the reference from escaping the scope of `runSTArray`.
foreign import runSTArray :: forall a r. (forall h. Eff (st :: ST h | r) (STArray h a)) -> Eff r (Array a)

-- | Create an empty mutable array.
foreign import emptySTArray :: forall a h r. Eff (st :: ST h | r) (STArray h a)

foreign import peekSTArrayImpl :: forall a h e r. Fn4 (a -> r)
                                                  r
                                                  (STArray h a)
                                                  Int
                                                  (Eff (st :: ST h | e) r)

-- | Read the value at the specified index in a mutable array.
peekSTArray :: forall a h r. STArray h a -> Int -> Eff (st :: ST h | r) (Maybe a)
peekSTArray = runFn4 peekSTArrayImpl Just Nothing

-- | Change the value at the specified index in a mutable array.
pokeSTArray :: forall a h r. STArray h a -> Int -> a -> Eff (st :: ST h | r) Boolean
pokeSTArray = runFn3 pokeSTArrayImpl

foreign import pokeSTArrayImpl :: forall a h e. Fn3 (STArray h a)
                                                    Int
                                                    a
                                                    (Eff (st :: ST h | e) Boolean)

-- | Append the values in an immutable array to the end of a mutable array.
pushAllSTArray :: forall a h r. STArray h a -> Array a -> Eff (st :: ST h | r) Int
pushAllSTArray = runFn2 pushAllSTArrayImpl

foreign import pushAllSTArrayImpl :: forall a h r. Fn2 (STArray h a)
                                                   (Array a)
                                                   (Eff (st :: ST h | r) Int)

-- | Append an element to the end of a mutable array.
pushSTArray :: forall a h r. STArray h a -> a -> Eff (st :: ST h | r) Int
pushSTArray arr a = pushAllSTArray arr [a]

-- | Remove and/or insert elements from/into a mutable array at the specified index.
spliceSTArray :: forall a h r. STArray h a -> Int -> Int -> Array a -> Eff (st :: ST h | r) (Array a)
spliceSTArray = runFn4 spliceSTArrayImpl

foreign import spliceSTArrayImpl :: forall a h r. Fn4 (STArray h a)
                                                      Int
                                                      Int
                                                      (Array a)
                                                      (Eff (st :: ST h | r) (Array a))

-- | Create an immutable copy of a mutable array.
freeze :: forall a h r. STArray h a -> Eff (st :: ST h | r) (Array a)
freeze = copyImpl

-- | Create a mutable copy of an immutable array.
thaw :: forall a h r. Array a -> Eff (st :: ST h | r) (STArray h a)
thaw = copyImpl

foreign import copyImpl :: forall a b h r. a -> Eff (st :: ST h | r) b

-- | Create an immutable copy of a mutable array, where each element
-- | is labelled with its index in the original array.
foreign import toAssocArray :: forall a h r. STArray h a -> Eff (st :: ST h | r) (Array (Assoc a))
