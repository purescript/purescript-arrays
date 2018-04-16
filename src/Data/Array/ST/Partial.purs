-- | Partial functions for working with mutable arrays using the `ST` effect.
-- |
-- | This module is particularly helpful when performance is very important.

module Data.Array.ST.Partial
  ( peekSTArray
  , pokeSTArray
  ) where

import Control.Monad.ST (ST)
import Data.Array.ST (STArray)
import Data.Unit (Unit)

-- | Read the value at the specified index in a mutable array.
peekSTArray
  :: forall h a
   . Partial
  => STArray h a
  -> Int
  -> ST h a
peekSTArray = peekSTArrayImpl

foreign import peekSTArrayImpl :: forall h a. STArray h a -> Int -> ST h a

-- | Change the value at the specified index in a mutable array.
pokeSTArray
  :: forall h a
   . Partial
  => STArray h a
  -> Int
  -> a
  -> ST h Unit
pokeSTArray = pokeSTArrayImpl

foreign import pokeSTArrayImpl
  :: forall h a
   . STArray h a
  -> Int
  -> a
  -> ST h Unit
