module Test.Data.Array.ST (testArrayST) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.ST (ST, pureST)
import Data.Array.ST (STArray, empty, freeze, peek, poke, pushAll, push, splice, thaw, toAssocArray, unsafeThaw, unsafeFreeze)
import Data.Foldable (all)
import Data.Maybe (Maybe(..), isNothing)
import Test.Assert (assert, ASSERT)

run :: forall a. (forall h. Eff (st :: ST h) (STArray h a)) -> Array a
run act = pureST (act >>= unsafeFreeze)

testArrayST :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
testArrayST = do

  log "empty should produce an empty array"

  assert $ run empty == nil

  log "thaw should produce an STArray from a standard array"

  assert $ run (thaw [1, 2, 3]) == [1, 2, 3]

  log "freeze should produce a standard array from an STArray"

  assert $ pureST (do
    arr <- thaw [1, 2, 3]
    freeze arr) == [1, 2, 3]

  log "unsafeThaw should produce an STArray from a standard array"

  assert $ run (unsafeThaw [1, 2, 3]) == [1, 2, 3]

  log "push should append a value to the end of the array"

  assert $ run (do
    arr <- empty
    void $ push arr 1
    void $ push arr 2
    pure arr) == [1, 2]

  assert $ run (do
    arr <- thaw [1, 2, 3]
    void $ push arr 4
    pure arr) == [1, 2, 3, 4]

  log "push should return the new length of the array"

  assert $ pureST (do
    arr <- thaw [unit, unit, unit]
    push arr unit) == 4

  log "pushAll should append multiple values to the end of the array"

  assert $ run (do
    arr <- empty
    void $ pushAll arr [1, 2]
    pure arr) == [1, 2]

  assert $ run (do
    arr <- thaw [1, 2, 3]
    void $ pushAll arr [4, 5, 6]
    pure arr) == [1, 2, 3, 4, 5, 6]

  log "pushAll should return the new length of the array"

  assert $ pureST (do
    arr <- thaw [unit, unit, unit]
    pushAll arr [unit, unit]) == 5

  log "peek should return Nothing when peeking a value outside the array bounds"

  assert $ isNothing $ pureST (do
    arr <- empty
    peek arr 0)

  assert $ isNothing $ pureST (do
    arr <- thaw [1]
    peek arr 1)

  assert $ isNothing $ pureST (do
    arr <- empty
    peek arr (-1))

  log "peek should return the value at the specified index"

  assert $ pureST (do
    arr <- thaw [1]
    peek arr 0) == Just 1

  assert $ pureST (do
    arr <- thaw [1, 2, 3]
    peek arr 2) == Just 3

  log "poke should return true when a value has been updated succesfully"

  assert $ pureST (do
    arr <- thaw [1]
    poke arr 0 10)

  assert $ pureST (do
    arr <- thaw [1, 2, 3]
    poke arr 2 30)

  log "poke should return false when attempting to modify a value outside the array bounds"

  assert $ not $ pureST (do
    arr <- empty
    poke arr 0 10)

  assert $ not $ pureST (do
    arr <- thaw [1, 2, 3]
    poke arr 3 100)

  assert $ not $ pureST (do
    arr <- thaw [1, 2, 3]
    poke arr (-1) 100)

  log "poke should replace the value at the specified index"

  assert $ run (do
    arr <- thaw [1]
    void $ poke arr 0 10
    pure arr) == [10]

  log "poke should do nothing when attempting to modify a value outside the array bounds"

  assert $ run (do
    arr <- thaw [1]
    void $ poke arr 1 2
    pure arr) == [1]

  log "splice should be able to delete multiple items at a specified index"

  assert $ run (do
    arr <- thaw [1, 2, 3, 4, 5]
    void $ splice arr 1 3 []
    pure arr) == [1, 5]

  log "splice should return the items removed"

  assert $ pureST (do
    arr <- thaw [1, 2, 3, 4, 5]
    splice arr 1 3 []) == [2, 3, 4]

  log "splice should be able to insert multiple items at a specified index"

  assert $ run (do
    arr <- thaw [1, 2, 3, 4, 5]
    void $ splice arr 1 0 [0, 100]
    pure arr) == [1, 0, 100, 2, 3, 4, 5]

  log "splice should be able to delete and insert at the same time"

  assert $ run (do
    arr <- thaw [1, 2, 3, 4, 5]
    void $ splice arr 1 2 [0, 100]
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
