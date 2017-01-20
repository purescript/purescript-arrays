module Test.Data.Array.ST (testArrayST) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.ST (ST, pureST)
import Data.Array.ST (STArray, emptySTArray, freeze, peekSTArray, pokeSTArray, pushAllSTArray, pushSTArray, spliceSTArray, thaw, toAssocArray, unsafeFreeze)
import Data.Foldable (all)
import Data.Maybe (Maybe(..), isNothing)
import Test.Assert (assert, ASSERT)

run :: forall a. (forall h. Eff (st :: ST h) (STArray h a)) -> Array a
run act = pureST (act >>= unsafeFreeze)

testArrayST :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
testArrayST = do

  log "emptySTArray should produce an empty array"

  assert $ run emptySTArray == nil

  log "thaw should produce an STArray from a standard array"

  assert $ run (thaw [1, 2, 3]) == [1, 2, 3]

  log "freeze should produce a standard array from an STArray"

  assert $ pureST (do
    arr <- thaw [1, 2, 3]
    freeze arr) == [1, 2, 3]

  log "pushSTArray should append a value to the end of the array"

  assert $ run (do
    arr <- emptySTArray
    pushSTArray arr 1
    pushSTArray arr 2
    pure arr) == [1, 2]

  assert $ run (do
    arr <- thaw [1, 2, 3]
    pushSTArray arr 4
    pure arr) == [1, 2, 3, 4]

  log "pushAllSTArray should append multiple values to the end of the array"

  assert $ run (do
    arr <- emptySTArray
    pushAllSTArray arr [1, 2]
    pure arr) == [1, 2]

  assert $ run (do
    arr <- thaw [1, 2, 3]
    pushAllSTArray arr [4, 5, 6]
    pure arr) == [1, 2, 3, 4, 5, 6]

  log "peekSTArray should return Nothing when peeking a value outside the array bounds"

  assert $ isNothing $ pureST (do
    arr <- emptySTArray
    peekSTArray arr 0)

  assert $ isNothing $ pureST (do
    arr <- thaw [1]
    peekSTArray arr 1)

  assert $ isNothing $ pureST (do
    arr <- emptySTArray
    peekSTArray arr (-1))

  log "peekSTArray should return the value at the specified index"

  assert $ pureST (do
    arr <- thaw [1]
    peekSTArray arr 0) == Just 1

  assert $ pureST (do
    arr <- thaw [1, 2, 3]
    peekSTArray arr 2) == Just 3

  log "pokeSTArray should return true when a value has been updated succesfully"

  assert $ pureST (do
    arr <- thaw [1]
    pokeSTArray arr 0 10)

  assert $ pureST (do
    arr <- thaw [1, 2, 3]
    pokeSTArray arr 2 30)

  log "pokeSTArray should return false when attempting to modify a value outside the array bounds"

  assert $ not $ pureST (do
    arr <- emptySTArray
    pokeSTArray arr 0 10)

  assert $ not $ pureST (do
    arr <- thaw [1, 2, 3]
    pokeSTArray arr 3 100)

  assert $ not $ pureST (do
    arr <- thaw [1, 2, 3]
    pokeSTArray arr (-1) 100)

  log "pokeSTArray should replace the value at the specified index"

  assert $ run (do
    arr <- thaw [1]
    pokeSTArray arr 0 10
    pure arr) == [10]

  log "pokeSTArray should do nothing when attempting to modify a value outside the array bounds"

  assert $ run (do
    arr <- thaw [1]
    pokeSTArray arr 1 2
    pure arr) == [1]

  log "spliceSTArray should be able to delete multiple items at a specified index"

  assert $ run (do
    arr <- thaw [1, 2, 3, 4, 5]
    spliceSTArray arr 1 3 []
    pure arr) == [1, 5]

  log "spliceSTArray should return the items removed"

  assert $ pureST (do
    arr <- thaw [1, 2, 3, 4, 5]
    spliceSTArray arr 1 3 []) == [2, 3, 4]

  log "spliceSTArray should be able to insert multiple items at a specified index"

  assert $ run (do
    arr <- thaw [1, 2, 3, 4, 5]
    spliceSTArray arr 1 0 [0, 100]
    pure arr) == [1, 0, 100, 2, 3, 4, 5]

  log "spliceSTArray should be able to delete and insert at the same time"

  assert $ run (do
    arr <- thaw [1, 2, 3, 4, 5]
    spliceSTArray arr 1 2 [0, 100]
    pure arr) == [1, 0, 100, 4, 5]

  log "toAssocArray should return all items in the array with the correct indices and values"

  assert $ all (\{ value: v, index: i } -> v == i + 1) $ pureST (do
    arr <- thaw [1, 2, 3, 4, 5]
    toAssocArray arr)

  assert $ all (\{ value: v, index: i } -> v == (i + 1) * 10) $ pureST (do
    arr <- thaw [10, 20, 30, 40, 50]
    toAssocArray arr)

nil :: Array Int
nil = []
