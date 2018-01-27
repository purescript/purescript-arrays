module Test.Data.Array.ST.Partial (testArraySTPartial) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.ST (pureST)

import Data.Array.ST (thaw, unsafeFreeze)
import Data.Array.ST.Partial (peek, poke)

import Partial.Unsafe (unsafePartial)

import Test.Assert (assert, ASSERT)

testArraySTPartial :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
testArraySTPartial = do

  log "peek should return the value at the specified index"
  assert $ 2 == pureST do
    a <- thaw [1, 2, 3]
    unsafePartial $ peek a 1

  log "poke should modify the value at the specified index"
  assert $ [1, 4, 3] == pureST do
    a <- thaw [1, 2, 3]
    unsafePartial $ poke a 1 4
    unsafeFreeze a
