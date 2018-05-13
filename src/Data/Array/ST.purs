-- | Helper functions for working with mutable arrays using the `ST` effect.
-- |
-- | This module can be used when performance is important and mutation is a local effect.

module Data.Array.ST
  ( STArray(..)
  , Assoc()
  , withArray
  , empty
  , peek
  , poke
  , push
  , pushAll
  , modify
  , splice
  , freeze
  , thaw
  , unsafeFreeze
  , unsafeThaw
  , toAssocArray

    -- deprecated
  , runSTArray
  , emptySTArray
  , peekSTArray
  , pokeSTArray
  , pushSTArray
  , pushAllSTArray
  , modifySTArray
  , spliceSTArray

  ) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.ST (ST)

import Data.Maybe (Maybe(..))
import Unsafe.Coerce (unsafeCoerce)

-- | A reference to a mutable array.
-- |
-- | The first type parameter represents the memory region which the array belongs to.
-- | The second type parameter defines the type of elements of the mutable array.
-- |
-- | The runtime representation of a value of type `STArray h a` is the same as that of `Array a`,
-- | except that mutation is allowed.
foreign import data STArray :: Type -> Type -> Type

-- | An element and its index.
type Assoc a = { value :: a, index :: Int }

-- | **DEPRECATED**: Use `unsafeFreeze` together with `runST` instead.
-- |
-- | Freeze a mutable array, creating an immutable array. Use this function as you would use
-- | `runST` to freeze a mutable reference.
-- |
-- | The rank-2 type prevents the reference from escaping the scope of `runSTArray`.
foreign import runSTArray
  :: forall a r
   . Warn "Deprecated `runSTArray`, use `unsafeFreeze` with `runST` instead."
  => (forall h. Eff (st :: ST h | r) (STArray h a))
  -> Eff r (Array a)

-- | Perform an effect requiring a mutable array on a copy of an immutable array,
-- | safely returning the result as an immutable array.
withArray
  :: forall a b r h
   . (STArray h a -> Eff (st :: ST h | r) b)
   -> Array a
   -> Eff (st :: ST h | r) (Array a)
withArray f xs = do
  result <- thaw xs
  _ <- f result
  unsafeFreeze result

-- | O(1). Convert a mutable array to an immutable array, without copying. The mutable
-- | array must not be mutated afterwards.
unsafeFreeze :: forall a r h. STArray h a -> Eff (st :: ST h | r) (Array a)
unsafeFreeze = pure <<< (unsafeCoerce :: STArray h a -> Array a)

-- | O(1) Convert an immutable array to a mutable array, without copying. The input
-- | array must not be used afterward.
unsafeThaw :: forall a r h. Array a -> Eff (st :: ST h | r) (STArray h a)
unsafeThaw = pure <<< (unsafeCoerce :: Array a -> STArray h a)

-- | Create an empty mutable array.
foreign import empty :: forall a h r. Eff (st :: ST h | r) (STArray h a)

emptySTArray
  :: forall a h r
   . Warn "Deprecated `emptySTArray`, use `empty` instead."
  => Eff (st :: ST h | r) (STArray h a)
emptySTArray = empty

-- | Create a mutable copy of an immutable array.
thaw :: forall a h r. Array a -> Eff (st :: ST h | r) (STArray h a)
thaw = copyImpl

-- | Create an immutable copy of a mutable array.
freeze :: forall a h r. STArray h a -> Eff (st :: ST h | r) (Array a)
freeze = copyImpl

foreign import copyImpl :: forall a b h r. a -> Eff (st :: ST h | r) b

-- | Read the value at the specified index in a mutable array.
peek
  :: forall a h r
   . STArray h a
  -> Int
  -> Eff (st :: ST h | r) (Maybe a)
peek = peekImpl Just Nothing

foreign import peekImpl
  :: forall a h e r
   . (a -> r)
  -> r
  -> STArray h a
  -> Int
  -> (Eff (st :: ST h | e) r)

peekSTArray
  :: forall a h r
   . Warn "Deprecated `peekSTArray`, use `peek` instead."
  => STArray h a
  -> Int
  -> Eff (st :: ST h | r) (Maybe a)
peekSTArray = peek

-- | Change the value at the specified index in a mutable array.
foreign import poke
  :: forall a h r
   . STArray h a
  -> Int
  -> a
  -> Eff (st :: ST h | r) Boolean

pokeSTArray
  :: forall a h r
   . Warn "Deprecated `pokeSTArray`, use `poke` instead."
  => STArray h a
  -> Int
  -> a
  -> Eff (st :: ST h | r) Boolean
pokeSTArray = poke

-- | Append an element to the end of a mutable array. Returns the new length of
-- | the array.
push :: forall a h r. STArray h a -> a -> Eff (st :: ST h | r) Int
push arr a = pushAll arr [a]

pushSTArray
  :: forall a h r
   . Warn "Deprecated `pushSTArray`, use `push` instead."
  => STArray h a
  -> a
  -> Eff (st :: ST h | r) Int
pushSTArray = push

-- | Append the values in an immutable array to the end of a mutable array.
-- | Returns the new length of the mutable array.
foreign import pushAll
  :: forall a h r
   . STArray h a
  -> Array a
  -> Eff (st :: ST h | r) Int

pushAllSTArray
  :: forall a h r
   . Warn "Deprecated `pushAllSTArray`, use `pushAll` instead."
  => STArray h a
  -> Array a
  -> Eff (st :: ST h | r) Int
pushAllSTArray = pushAll

-- | Mutate the element at the specified index using the supplied function.
modify
  :: forall a h r
   . STArray h a
  -> Int
  -> (a -> a)
  -> Eff (st :: ST h | r) Boolean
modify xs i f = do
  entry <- peek xs i
  case entry of
    Just x  -> poke xs i (f x)
    Nothing -> pure false

modifySTArray
  :: forall a h r
   . Warn "Deprecated `modifySTArray`, use `modify` instead."
  => STArray h a
  -> Int
  -> (a -> a)
  -> Eff (st :: ST h | r) Boolean
modifySTArray = modify

-- | Remove and/or insert elements from/into a mutable array at the specified index.
foreign import splice
  :: forall a h r
   . STArray h a
  -> Int
  -> Int
  -> Array a
  -> Eff (st :: ST h | r) (Array a)

spliceSTArray
  :: forall a h r
   . Warn "Deprecated `spliceSTArray`, use `splice` instead."
  => STArray h a
  -> Int
  -> Int
  -> Array a
  -> Eff (st :: ST h | r) (Array a)
spliceSTArray = splice

-- | Create an immutable copy of a mutable array, where each element
-- | is labelled with its index in the original array.
foreign import toAssocArray
  :: forall a h r
   . STArray h a
  -> Eff (st :: ST h | r) (Array (Assoc a))
