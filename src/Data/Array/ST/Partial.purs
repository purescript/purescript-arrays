-- | Partial functions for working with mutable arrays using the `ST` effect.
-- |
-- | This module is particularly helpful when performance is very important.

module Data.Array.ST.Partial
  ( peek
  , poke

  -- deprecated
  , peekSTArray
  , pokeSTArray
  ) where

import Control.Monad.Eff (Eff)
import Control.Monad.ST (ST)
import Data.Array.ST (STArray)
import Data.Unit (Unit)

-- | Read the value at the specified index in a mutable array.
peek
  :: forall a h r
   . Partial
  => STArray h a
  -> Int
  -> Eff (st :: ST h | r) a
peek = peekImpl

peekSTArray
  :: forall a h r
   . Warn "Deprecated `peekSTArray`, use `peek` instead."
  => Partial
  => STArray h a
  -> Int
  -> Eff (st :: ST h | r) a
peekSTArray = peek

foreign import peekImpl
  :: forall a h r
   . STArray h a
  -> Int
  -> Eff (st :: ST h | r) a

-- | Change the value at the specified index in a mutable array.
poke
  :: forall a h r
   . Partial
  => STArray h a
  -> Int
  -> a
  -> Eff (st :: ST h | r) Unit
poke = pokeImpl

pokeSTArray
  :: forall a h r
   . Warn "Deprecated `peekSTArray`, use `peek` instead."
  => Partial
  => STArray h a
  -> Int
  -> a
  -> Eff (st :: ST h | r) Unit
pokeSTArray = poke

foreign import pokeImpl
  :: forall a h r
   . STArray h a
  -> Int
  -> a
  -> Eff (st :: ST h | r) Unit
