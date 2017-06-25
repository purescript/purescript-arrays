-- | Partial functions for working with mutable arrays using the `ST` effect.
-- |
-- | This module is particularly helpful when performance is very important.

module Data.Array.ST.Partial
  ( peekSTArray
  , pokeSTArray
  ) where

import Control.Monad.Eff (Eff)
import Control.Monad.ST (ST)
import Data.Array.ST (STArray)
import Data.Unit (Unit)

-- | Read the value at the specified index in a mutable array.
peekSTArray
  :: forall a h r
   . Partial
  => STArray h a
  -> Int
  -> Eff (st :: ST h | r) a
peekSTArray = peekSTArrayImpl

foreign import peekSTArrayImpl
  :: forall a h r
   . STArray h a
  -> Int
  -> Eff (st :: ST h | r) a

-- | Change the value at the specified index in a mutable array.
pokeSTArray
  :: forall a h r
   . Partial
  => STArray h a
  -> Int
  -> a
  -> Eff (st :: ST h | r) Unit
pokeSTArray = pokeSTArrayImpl

foreign import pokeSTArrayImpl
  :: forall a h r
   . STArray h a
  -> Int
  -> a
  -> Eff (st :: ST h | r) Unit