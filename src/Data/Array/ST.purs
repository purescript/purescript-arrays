-- | Helper functions for working with mutable arrays using the `ST` effect.
-- |
-- | This module can be used when performance is important and mutation is a local effect.

module Data.Array.ST
  ( STArray(..)
  , Assoc()
  , runSTArray
  , runSTArray'
  , emptySTArray
  , peekSTArray
  , pokeSTArray
  , pushSTArray
  , pushAllSTArray
  , spliceSTArray
  , freeze, thaw
  , toAssocArray
  ) where

import Prelude
import Control.Monad.Eff (Eff, Pure, runPure)
import Control.Monad.ST (ST)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Unsafe.Coerce (unsafeCoerce)

-- | A reference to a mutable array.
-- |
-- | The first type parameter represents the memory region which the array belongs to.
-- | The second type parameter defines the type of elements of the mutable array.
-- |
-- | The runtime representation of a value of type `STArray h a` is the same as that of `Array a`,
-- | except that mutation is allowed.
foreign import data STArray :: * -> * -> *

-- | An element and its index.
type Assoc a = { value :: a, index :: Int }

newtype Id a f = Id (f a)

derive instance newtypeId :: Newtype (Id a f) _

-- | Freeze a mutable array, creating an immutable array. Use this function as you would use
-- | `runST` to freeze a mutable reference.
-- |
-- | The rank-2 type prevents the reference from escaping the scope of `runSTArray'`,
-- | and the closed row on the `Eff` computation prevents the reference from
-- | escaping into other parts of your program via native effects such as `setTimeout`.
-- |
-- | You can also return an immutable copy of an `STArray` from an `ST` computation
-- | by using `freeze` combined with `runST`. However, when possible, you should
-- | prefer this function, because it is `O(1)`. By contrast, `freeze` must copy the
-- | underlying array and is therefore `O(n)`.
runSTArray
  :: forall a
   . (forall h. Eff (st :: ST h) (STArray h a))
  -> Array a
runSTArray a = unwrap (runSTArray' (map Id a))

-- | Freeze all mutable arrays in some structure, creating a version of the
-- | same structure where all mutable arrays are replaced with immutable
-- | arrays. Use this function as you would use `runST` to freeze a mutable
-- | reference.
-- |
-- | The rank-2 type prevents the reference from escaping the scope of `runSTArray'`,
-- | and the closed row on the `Eff` computation prevents the reference from
-- | escaping into other parts of your program via native effects such as `setTimeout`.
-- |
-- | You can also return an immutable copy of an `STArray` from an `ST` computation
-- | by using `freeze` combined with `runST`. However, when possible, you should
-- | prefer this function, because it is `O(1)`. By contrast, `freeze` must copy the
-- | underlying array and is therefore `O(n)`.
runSTArray'
  :: forall f
   . (forall h. Eff (st :: ST h) (f (STArray h)))
  -> f Array
runSTArray' x = runPure (go x)
  where
  go :: (forall h. Eff (st :: ST h) (f (STArray h))) -> Pure (f Array)
  go = unsafeCoerce

-- | Create an empty mutable array.
foreign import emptySTArray :: forall a h r. Eff (st :: ST h | r) (STArray h a)

-- | Create a mutable copy of an immutable array.
thaw :: forall a h r. Array a -> Eff (st :: ST h | r) (STArray h a)
thaw = copyImpl

-- | Create an immutable copy of a mutable array.
freeze :: forall a h r. STArray h a -> Eff (st :: ST h | r) (Array a)
freeze = copyImpl

foreign import copyImpl :: forall a b h r. a -> Eff (st :: ST h | r) b

-- | Read the value at the specified index in a mutable array.
peekSTArray
  :: forall a h r
   . STArray h a
  -> Int
  -> Eff (st :: ST h | r) (Maybe a)
peekSTArray = peekSTArrayImpl Just Nothing

foreign import peekSTArrayImpl
  :: forall a h e r
   . (a -> r)
  -> r
  -> (STArray h a)
  -> Int
  -> (Eff (st :: ST h | e) r)

-- | Change the value at the specified index in a mutable array.
foreign import pokeSTArray
  :: forall a h r
   . STArray h a -> Int -> a -> Eff (st :: ST h | r) Boolean

-- | Append an element to the end of a mutable array.
pushSTArray :: forall a h r. STArray h a -> a -> Eff (st :: ST h | r) Int
pushSTArray arr a = pushAllSTArray arr [a]

-- | Append the values in an immutable array to the end of a mutable array.
foreign import pushAllSTArray
  :: forall a h r
   . STArray h a
  -> Array a
  -> Eff (st :: ST h | r) Int

-- | Remove and/or insert elements from/into a mutable array at the specified index.
foreign import spliceSTArray
  :: forall a h r
   . STArray h a
  -> Int
  -> Int
  -> Array a
  -> Eff (st :: ST h | r) (Array a)

-- | Create an immutable copy of a mutable array, where each element
-- | is labelled with its index in the original array.
foreign import toAssocArray
  :: forall a h r
   . STArray h a
  -> Eff (st :: ST h | r) (Array (Assoc a))
