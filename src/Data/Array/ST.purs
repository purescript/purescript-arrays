module Data.Array.ST 
  ( STArray(..)
  , runSTArray
  , emptySTArray
  , peekSTArray
  , pokeSTArray  
  , pushSTArray  
  ) where

import Data.Maybe
import Data.Function

import Control.Monad.Eff
import Control.Monad.ST (ST())

foreign import data STArray :: * -> * -> *

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

foreign import pushSTArrayImpl """
  function pushSTArrayImpl(arr, a) {
    return function() {
      arr.push(a);
      return {};
    };
  }""" :: forall a h e. Fn2 (STArray h a)
                            a
                            (Eff (st :: ST h | e) Unit)
                            
pushSTArray :: forall a h r. STArray h a -> a -> Eff (st :: ST h | r) Unit
pushSTArray arr a = runFn2 pushSTArrayImpl arr a
