module Test.Data.Array.ST.Partial (testArraySTPartial) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.ST (pureST)

import Data.Array.ST (thaw, unsafeFreeze)
import Data.Array.ST.Partial (peekSTArray, pokeSTArray)

import Partial.Unsafe (unsafePartial)

import Test.Assert (assert, ASSERT)

testArraySTPartial :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
testArraySTPartial = do

  log "peekSTArray should return the value at the specified index"
  assert $ 2 == pureST do
    a <- thaw [1, 2, 3]
    unsafePartial $ peekSTArray a 1

  log "pokeSTArray should modify the value at the specified index"
  assert $ [1, 4, 3] == pureST do
    a <- thaw [1, 2, 3]
    unsafePartial $ pokeSTArray a 1 4
    unsafeFreeze a