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
  , getElems
  , getAssocs
  ) where

import Data.Maybe
import Data.Function

import Control.Monad.Eff
import Control.Monad.ST (ST())

foreign import data STArray :: * -> * -> *

type Assoc a = { value :: a, index :: Number }

foreign import runSTArray """
  function runSTArray(f) {
    return f;
  }""" :: forall a r. (forall h. Eff (st :: ST h | r) (STArray h a)) -> Eff r [a]

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

pushAllSTArray :: forall a h r. STArray h a -> [a] -> Eff (st :: ST h | r) Number
pushAllSTArray = runFn2 pushAllSTArrayImpl

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

spliceSTArray :: forall a h r. STArray h a -> Number -> Number -> [a] -> Eff (st :: ST h | r) [a]
spliceSTArray = runFn4 spliceSTArrayImpl

foreign import getElems """
  function getElems(arr) {
    return function(){
      var as = [];
      var i = -1;
      var n = arr.length;
      while(++i < n) {
        as[i] = arr[i];
      }
      return as;
    };
  }""" :: forall a h r. STArray h a -> Eff (st :: ST h | r) [a]

foreign import getAssocs """
  function getAssocs(arr) {
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
