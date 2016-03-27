module Test.Data.Array.Partial (testArrayPartial) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)

import Data.Array.Partial (init, last, tail, head)

import Partial.Unsafe (unsafePartial)

import Test.Assert (assert, ASSERT)

testArrayPartial :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
testArrayPartial = do

  log "head should return the first item in an array"
  assert $ unsafePartial $ head [1, 2, 3] == 1
  assert $ unsafePartial $ head [1] == 1

  log "tail should return all but the first item in an array"
  assert $ unsafePartial $ tail [1, 2, 3] == [2, 3]

  log "last should return the last item of an array"
  assert $ unsafePartial $ last [1, 2, 3] == 3
  assert $ unsafePartial $ last [1] == 1

  log "init should return all but the last item of an array"
  assert $ unsafePartial $ init [1, 2, 3] == [1, 2]
