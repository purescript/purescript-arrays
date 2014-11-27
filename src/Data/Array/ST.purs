module Data.Array.ST 
  ( STArray(..)
  , runSTArray
  , emptySTArray
  , peekSTArray
  , pokeSTArray  
  ) where

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
  }""" :: forall a h r. Number -> a -> Eff (st :: ST h | r) (STArray h a)
  
foreign import peekSTArray """
  function peekSTArray(arr) {
    return function(i) {
      return function() {
        return arr[i];
      };
    };
  }""" :: forall a h r. STArray h a -> Number -> Eff (st :: ST h | r) a
  
foreign import pokeSTArray """
  function pokeSTArray(arr) {
    return function(i) {
      return function(a) {
        return function() {
          arr[i] = a;
          return {};
        };
      };
    };
  }""" :: forall a h r. STArray h a -> Number -> a -> Eff (st :: ST h | r) Unit
