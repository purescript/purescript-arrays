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

import Data.Maybe
import Data.Function

import Control.Monad.Eff
import Control.Monad.ST (ST())

-- | A reference to a mutable array.
-- |
-- | The first type parameter represents the memory region which the array belongs to.
-- | The second type parameter defines the type of elements of the mutable array.
-- |
-- | The runtime representation of a value of type `STArray h a` is the same as that of `[a]`,
-- | except that mutation is allowed.
foreign import data STArray :: * -> * -> *

-- | An element and its index
type Assoc a = { value :: a, index :: Number }

-- | Freeze a mutable array, creating an immutable array. Use this function as you would use
-- | `runST` to freeze a mutable reference.
-- |
-- | The rank-2 type prevents the reference from escaping the scope of `runSTArray`.
foreign import runSTArray """
  function runSTArray(f) {
    return f;
  }""" :: forall a r. (forall h. Eff (st :: ST h | r) (STArray h a)) -> Eff r [a]

-- | Create an empty mutable array.
foreign import emptySTArray """
  function emptySTArray() {
    return [];
  }""" :: forall a h r. Eff (st :: ST h | r) (STArray h a)

foreign import peekSTArrayImpl """
  function peekSTArrayImpl(arr, i, s, f) {
    return function() {
      var index = ~~i;
      if (0 <= index && index < arr.length) {
        return s(arr[index]);
      } else {
        return f;
      }
    };
  }""" :: forall a h e r. Fn4 (STArray h a)
                              Number
                              (a -> r)
                              r
                              (Eff (st :: ST h | e) r)

-- | Read the value at the specified index in a mutable array.
peekSTArray :: forall a h r. STArray h a -> Number -> Eff (st :: ST h | r) (Maybe a)
peekSTArray arr i = runFn4 peekSTArrayImpl arr i Just Nothing

foreign import pokeSTArrayImpl """
  function pokeSTArrayImpl(arr, i, a) {
    return function() {
      var index = ~~i;
      if (0 <= index && index <= arr.length) {
        arr[index] = a;
        return true;
      }
      return false;
    };
  }""" :: forall a h e. Fn3 (STArray h a)
                            Number
                            a
                            (Eff (st :: ST h | e) Boolean)

-- | Change the value at the specified index in a mutable array.
pokeSTArray :: forall a h r. STArray h a -> Number -> a -> Eff (st :: ST h | r) Boolean
pokeSTArray arr i a = runFn3 pokeSTArrayImpl arr i a

foreign import pushAllSTArrayImpl """
  function pushAllSTArrayImpl(arr, as) {
    return function(){
      return arr.push.apply(arr, as);
    };
  }""" :: forall a h r. Fn2 (STArray h a)
                            [a]
                            (Eff (st :: ST h | r) Number)

-- | Append the values in an immutable array to the end of a mutable array.
pushAllSTArray :: forall a h r. STArray h a -> [a] -> Eff (st :: ST h | r) Number
pushAllSTArray = runFn2 pushAllSTArrayImpl

-- | Append an element to the end of a mutable array.
pushSTArray :: forall a h r. STArray h a -> a -> Eff (st :: ST h | r) Number
pushSTArray arr a = pushAllSTArray arr [a]

foreign import spliceSTArrayImpl """
  function spliceSTArrayImpl(arr, index, howMany, bs) {
    return function(){
      return arr.splice.apply(arr, [index, howMany].concat(bs));
    };
  }""" :: forall a h r. Fn4 (STArray h a)
                            Number
                            Number
                            [a]
                            (Eff (st :: ST h | r) [a])

-- | Remove and/or insert elements from/into a mutable array at the specified index.
spliceSTArray :: forall a h r. STArray h a -> Number -> Number -> [a] -> Eff (st :: ST h | r) [a]
spliceSTArray = runFn4 spliceSTArrayImpl

foreign import copyImpl """
  function copyImpl(arr) {
    return function(){
      var as = [];
      var i = -1;
      var n = arr.length;
      while(++i < n) {
        as[i] = arr[i];
      }
      return as;
    };
  }""" :: forall a b h r. a -> Eff (st :: ST h | r) b

-- | Create an immutable copy of a mutable array.
freeze :: forall a h r. STArray h a -> Eff (st :: ST h | r) [a]
freeze = copyImpl

-- | Create a mutable copy of an immutable array.
thaw :: forall a h r. [a] -> Eff (st :: ST h | r) (STArray h a)
thaw = copyImpl

-- | Create an immutable copy of a mutable array, where each element
-- | is labelled with its index in the original array.
foreign import toAssocArray """
  function toAssocArray(arr) {
    return function(){
      var as = [];
      var i = -1;
      var n = arr.length;
      while(++i < n) {
        as[i] = {value: arr[i], index: i};
      }
      return as;
    };
  }""" :: forall a h r. STArray h a -> Eff (st :: ST h | r) [Assoc a]
