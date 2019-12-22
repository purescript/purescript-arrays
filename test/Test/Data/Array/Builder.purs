module Test.Data.Array.Builder (testArrayBuilder) where

import Prelude

import Data.Array.Builder (build, cons, snoc)
import Effect (Effect)
import Effect.Console (log)
import Test.Assert (assert)

testArrayBuilder :: Effect Unit
testArrayBuilder = do

  log "cons chain builds array according to the call order"
  assert $ build (cons 1 <> cons 2 <> cons 3) == [1, 2, 3]

  log "snoc chain builds array with reverse order"
  assert $ build (snoc 1 <> snoc 2 <> snoc 3) == [3, 2, 1]
