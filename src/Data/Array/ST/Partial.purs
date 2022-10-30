-- | Partial functions for working with mutable arrays using the `ST` effect.
-- |
-- | This module is particularly helpful when performance is very important.

module Data.Array.ST.Partial
  ( peek
  , poke
  ) where

import Control.Monad.ST (ST)
import Control.Monad.ST.Uncurried (STFn2, STFn3, runSTFn2, runSTFn3)
import Data.Array.ST (STArray)
import Data.Unit (Unit)

-- | Read the value at the specified index in a mutable array.
peek
  :: forall h a
   . Partial
  => Int
  -> STArray h a
  -> ST h a
peek = runSTFn2 peekImpl

foreign import peekImpl :: forall h a. STFn2 Int (STArray h a) h a

-- | Change the value at the specified index in a mutable array.
poke
  :: forall h a
   . Partial
  => Int
  -> a
  -> STArray h a
  -> ST h Unit
poke = runSTFn3 pokeImpl

foreign import pokeImpl :: forall h a. STFn3 Int a (STArray h a) h Unit
