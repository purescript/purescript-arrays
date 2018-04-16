-- | Helper functions for working with mutable arrays using the `ST` effect.
-- |
-- | This module can be used when performance is important and mutation is a local effect.

module Data.Array.ST
  ( STArray(..)
  , Assoc
  , withArray
  , emptySTArray
  , peekSTArray
  , pokeSTArray
  , pushSTArray
  , modifySTArray
  , pushAllSTArray
  , spliceSTArray
  , freeze
  , thaw
  , unsafeFreeze
  , unsafeThaw
  , toAssocArray
  ) where

import Prelude

import Control.Monad.ST (ST, kind Region)
import Data.Maybe (Maybe(..))
import Unsafe.Coerce (unsafeCoerce)

-- | A reference to a mutable array.
-- |
-- | The first type parameter represents the memory region which the array belongs to.
-- | The second type parameter defines the type of elements of the mutable array.
-- |
-- | The runtime representation of a value of type `STArray h a` is the same as that of `Array a`,
-- | except that mutation is allowed.
foreign import data STArray :: Region -> Type -> Type

-- | An element and its index.
type Assoc a = { value :: a, index :: Int }

-- | Perform an effect requiring a mutable array on a copy of an immutable array,
-- | safely returning the result as an immutable array.
withArray
  :: forall h a b
   . (STArray h a -> ST h b)
   -> Array a
   -> ST h (Array a)
withArray f xs = do
  result <- thaw xs
  _ <- f result
  unsafeFreeze result

-- | O(1). Convert a mutable array to an immutable array, without copying. The mutable
-- | array must not be mutated afterwards.
unsafeFreeze :: forall h a. STArray h a -> ST h (Array a)
unsafeFreeze = pure <<< (unsafeCoerce :: STArray h a -> Array a)

-- | O(1) Convert an immutable array to a mutable array, without copying. The input
-- | array must not be used afterward.
unsafeThaw :: forall h a. Array a -> ST h (STArray h a)
unsafeThaw = pure <<< (unsafeCoerce :: Array a -> STArray h a)

-- | Create an empty mutable array.
foreign import emptySTArray :: forall h a. ST h (STArray h a)

-- | Create a mutable copy of an immutable array.
thaw :: forall h a. Array a -> ST h (STArray h a)
thaw = copyImpl

-- | Create an immutable copy of a mutable array.
freeze :: forall h a. STArray h a -> ST h (Array a)
freeze = copyImpl

foreign import copyImpl :: forall h a b. a -> ST h b

-- | Read the value at the specified index in a mutable array.
peekSTArray
  :: forall h a
   . STArray h a
  -> Int
  -> ST h (Maybe a)
peekSTArray = peekSTArrayImpl Just Nothing

foreign import peekSTArrayImpl
  :: forall h a r
   . (a -> r)
  -> r
  -> STArray h a
  -> Int
  -> (ST h r)

-- | Change the value at the specified index in a mutable array.
foreign import pokeSTArray
  :: forall h a
   . STArray h a -> Int -> a -> ST h Boolean

-- | Append an element to the end of a mutable array. Returns the new length of
-- | the array.
pushSTArray :: forall h a. STArray h a -> a -> ST h Int
pushSTArray arr a = pushAllSTArray arr [a]

-- | Append the values in an immutable array to the end of a mutable array.
-- | Returns the new length of the mutable array.
foreign import pushAllSTArray
  :: forall h a
   . STArray h a
  -> Array a
  -> ST h Int

-- | Mutate the element at the specified index using the supplied function.
modifySTArray :: forall h a. STArray h a -> Int -> (a -> a) -> ST h Boolean
modifySTArray xs i f = do
  entry <- peekSTArray xs i
  case entry of
    Just x  -> pokeSTArray xs i (f x)
    Nothing -> pure false

-- | Remove and/or insert elements from/into a mutable array at the specified index.
foreign import spliceSTArray
  :: forall h a
   . STArray h a
  -> Int
  -> Int
  -> Array a
  -> ST h (Array a)

-- | Create an immutable copy of a mutable array, where each element
-- | is labelled with its index in the original array.
foreign import toAssocArray
  :: forall h a
   . STArray h a
  -> ST h (Array (Assoc a))
