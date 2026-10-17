module Test.Data.Array.ST (testArrayST) where

import Prelude

import Control.Monad.ST as ST
import Data.Array (range)
import Data.Array.ST (withArray)
import Data.Array.ST as STA
import Data.Foldable (all)
import Data.Maybe (Maybe(..), isNothing)
import Data.Tuple (Tuple(..), fst)
import Effect (Effect)
import Effect.Console (log)
import Test.Assert (assert)
import Test.Data.UndefinedOr (defined, undefined)

testArrayST :: Effect Unit
testArrayST = do

  log "run should produce an immutable array by running a constructor operation"

  let
    constructed = STA.run do
      arr <- STA.new
      void $ STA.push 1 arr
      void $ STA.push 2 arr
      pure arr
  assert $ constructed == [ 1, 2 ]

  log "withArray should run an operation on a copy of an array"

  let original = [ 1, 2, 3 ]
  assert $ ST.run (withArray (STA.push 42) original) == [ 1, 2, 3, 42 ]
  assert $ original == [ 1, 2, 3 ]

  log "empty should produce an empty array"

  assert $ STA.run STA.new == nil

  log "thaw should produce an STArray from a standard array"

  assert $ STA.run (STA.thaw [ 1, 2, 3 ]) == [ 1, 2, 3 ]

  log "freeze should produce a standard array from an STArray"

  let
    frozen = ST.run do
      arr <- STA.thaw [ 1, 2, 3 ]
      STA.freeze arr
  assert $ frozen == [ 1, 2, 3 ]

  log "clone should produce a shallow copy of an STArray"

  let
    cloned = ST.run do
      arr <- STA.thaw [ 1, 2, 3 ]
      arr2 <- STA.clone arr
      _ <- STA.poke 0 4 arr
      STA.freeze arr2
  assert $ cloned == [ 1, 2, 3 ]

  log "unsafeThaw should produce an STArray from a standard array"

  assert $ STA.run (STA.unsafeThaw [ 1, 2, 3 ]) == [ 1, 2, 3 ]

  log "length should return the number of items in an STArray"

  assert $ ST.run do
    arr <- STA.thaw nil
    length <- STA.length arr
    pure $ length == 0

  assert $ ST.run do
    arr <- STA.thaw [ 1 ]
    length <- STA.length arr
    pure $ length == 1

  assert $ ST.run do
    arr <- STA.thaw [ 1, 2, 3, 4, 5 ]
    length <- STA.length arr
    pure $ length == 5

  log "pop should remove elements from an STArray"

  let
    afterPop = STA.run do
      arr <- STA.thaw [ 1, 2, 3 ]
      void $ STA.pop arr
      pure arr
  assert $ afterPop == [ 1, 2 ]

  log "pop should return the last element of an STArray"

  let
    popped = ST.run do
      arr <- STA.thaw [ 1, 2, 3 ]
      STA.pop arr
  assert $ popped == Just 3

  log "pop should return Nothing when given an empty array"

  assert $ isNothing $ ST.run do
    arr <- STA.new
    STA.pop arr

  log "push should append a value to the end of the array"

  let
    pushedNew = STA.run do
      arr <- STA.new
      void $ STA.push 1 arr
      void $ STA.push 2 arr
      pure arr
  assert $ pushedNew == [ 1, 2 ]

  let
    pushedExisting = STA.run do
      arr <- STA.thaw [ 1, 2, 3 ]
      void $ STA.push 4 arr
      pure arr
  assert $ pushedExisting == [ 1, 2, 3, 4 ]

  log "push should return the new length of the array"

  let
    pushedLength = ST.run do
      arr <- STA.thaw [ unit, unit, unit ]
      STA.push unit arr
  assert $ pushedLength == 4

  log "pushAll should append multiple values to the end of the array"

  let
    pushedAllNew = STA.run do
      arr <- STA.new
      void $ STA.pushAll [ 1, 2 ] arr
      pure arr
  assert $ pushedAllNew == [ 1, 2 ]

  let
    pushedAllExisting = STA.run do
      arr <- STA.thaw [ 1, 2, 3 ]
      void $ STA.pushAll [ 4, 5, 6 ] arr
      pure arr
  assert $ pushedAllExisting == [ 1, 2, 3, 4, 5, 6 ]

  log "pushAll should return the new length of the array"

  let
    pushedAllLength = ST.run do
      arr <- STA.thaw [ unit, unit, unit ]
      STA.pushAll [ unit, unit ] arr
  assert $ pushedAllLength == 5

  log "peek should return Nothing when peeking a value outside the array bounds"

  assert $ isNothing $ ST.run do
    arr <- STA.new
    STA.peek 0 arr

  assert $ isNothing $ ST.run do
    arr <- STA.thaw [ 1 ]
    STA.peek 1 arr

  assert $ isNothing $ ST.run do
    arr <- STA.new
    STA.peek (-1) arr

  log "peek should return the value at the specified index"

  let
    peekedFirst = ST.run do
      arr <- STA.thaw [ 1 ]
      STA.peek 0 arr
  assert $ peekedFirst == Just 1

  let
    peekedLast = ST.run do
      arr <- STA.thaw [ 1, 2, 3 ]
      STA.peek 2 arr
  assert $ peekedLast == Just 3

  log "poke should return true when a value has been updated succesfully"

  assert $ ST.run do
    arr <- STA.thaw [ 1 ]
    STA.poke 0 10 arr

  assert $ ST.run do
    arr <- STA.thaw [ 1, 2, 3 ]
    STA.poke 2 30 arr

  log "poke should return false when attempting to modify a value outside the array bounds"

  assert $ not $ ST.run do
    arr <- STA.new
    STA.poke 0 10 arr

  assert $ not $ ST.run do
    arr <- STA.thaw [ 1, 2, 3 ]
    STA.poke 3 100 arr

  assert $ not $ ST.run do
    arr <- STA.thaw [ 1, 2, 3 ]
    STA.poke (-1) 100 arr

  log "poke should replace the value at the specified index"

  let
    poked = STA.run do
      arr <- STA.thaw [ 1 ]
      void $ STA.poke 0 10 arr
      pure arr
  assert $ poked == [ 10 ]

  log "poke should do nothing when attempting to modify a value outside the array bounds"

  let
    pokedOutOfBounds = STA.run do
      arr <- STA.thaw [ 1 ]
      void $ STA.poke 1 2 arr
      pure arr
  assert $ pokedOutOfBounds == [ 1 ]

  log "shift should remove elements from an STArray"

  let
    afterShift = STA.run do
      arr <- STA.thaw [ 1, 2, 3 ]
      void $ STA.shift arr
      pure arr
  assert $ afterShift == [ 2, 3 ]

  log "shift should return the first element of an STArray"

  let
    shifted = ST.run do
      arr <- STA.thaw [ 1, 2, 3 ]
      STA.shift arr
  assert $ shifted == Just 1

  log "shift should return Nothing when given an empty array"

  assert $ isNothing $ ST.run do
    arr <- STA.new
    STA.shift arr

  log "unshift should append a value to the front of the array"

  let
    unshiftedNew = STA.run do
      arr <- STA.new
      void $ STA.unshift 1 arr
      void $ STA.unshift 2 arr
      pure arr
  assert $ unshiftedNew == [ 2, 1 ]

  let
    unshiftedExisting = STA.run do
      arr <- STA.thaw [ 1, 2, 3 ]
      void $ STA.unshift 4 arr
      pure arr
  assert $ unshiftedExisting == [ 4, 1, 2, 3 ]

  log "unshift should return the new length of the array"

  let
    unshiftedLength = ST.run do
      arr <- STA.thaw [ unit, unit, unit ]
      STA.unshift unit arr
  assert $ unshiftedLength == 4

  log "unshiftAll should append multiple values to the front of the array"

  let
    unshiftedAllNew = STA.run do
      arr <- STA.new
      void $ STA.unshiftAll [ 1, 2 ] arr
      pure arr
  assert $ unshiftedAllNew == [ 1, 2 ]

  let
    unshiftedAllExisting = STA.run do
      arr <- STA.thaw [ 1, 2, 3 ]
      void $ STA.unshiftAll [ 4, 5, 6 ] arr
      pure arr
  assert $ unshiftedAllExisting == [ 4, 5, 6, 1, 2, 3 ]

  log "unshiftAll should return the new length of the array"

  let
    unshiftedAllLength = ST.run do
      arr <- STA.thaw [ unit, unit, unit ]
      STA.unshiftAll [ unit, unit ] arr
  assert $ unshiftedAllLength == 5

  log "sort should reorder a list into ascending order based on the result of compare"
  let sorted = STA.run (STA.sort =<< STA.unsafeThaw [ 1, 3, 2, 5, 6, 4 ])
  assert $ sorted == [ 1, 2, 3, 4, 5, 6 ]
  let sortedWithUndefined = STA.run (STA.sort =<< STA.unsafeThaw [ defined 1, undefined, defined 2 ])
  assert $ sortedWithUndefined == [ undefined, defined 1, defined 2 ]

  log "sortBy should reorder a list into ascending order based on the result of a comparison function"
  let sortedDescending = STA.run (STA.sortBy (flip compare) =<< STA.unsafeThaw [ 1, 3, 2, 5, 6, 4 ])
  assert $ sortedDescending == [ 6, 5, 4, 3, 2, 1 ]

  log "sortBy should not reorder elements that are equal according to a comparison function"
  let s1 = map (Tuple "a") (range 1 100)
  let sortedEqual = STA.run (STA.sortBy (comparing fst) =<< STA.unsafeThaw s1)
  assert $ sortedEqual == s1

  log "sortWith should reorder a list into ascending order based on the result of compare over a projection"
  let sortedWith = STA.run (STA.sortWith identity =<< STA.unsafeThaw [ 1, 3, 2, 5, 6, 4 ])
  assert $ sortedWith == [ 1, 2, 3, 4, 5, 6 ]

  log "sortWith should not reorder elements that are equal according to a projection"
  let s2 = map (Tuple "a") (range 1 100)
  let sortedWithEqual = STA.run (STA.sortWith fst =<< STA.unsafeThaw s2)
  assert $ sortedWithEqual == s2

  log "splice should be able to delete multiple items at a specified index"

  let
    splicedDeleted = STA.run do
      arr <- STA.thaw [ 1, 2, 3, 4, 5 ]
      void $ STA.splice 1 3 [] arr
      pure arr
  assert $ splicedDeleted == [ 1, 5 ]

  log "splice should return the items removed"

  let
    splicedRemoved = ST.run do
      arr <- STA.thaw [ 1, 2, 3, 4, 5 ]
      STA.splice 1 3 [] arr
  assert $ splicedRemoved == [ 2, 3, 4 ]

  log "splice should be able to insert multiple items at a specified index"

  let
    splicedInserted = STA.run do
      arr <- STA.thaw [ 1, 2, 3, 4, 5 ]
      void $ STA.splice 1 0 [ 0, 100 ] arr
      pure arr
  assert $ splicedInserted == [ 1, 0, 100, 2, 3, 4, 5 ]

  log "splice should be able to delete and insert at the same time"

  let
    splicedReplaced = STA.run do
      arr <- STA.thaw [ 1, 2, 3, 4, 5 ]
      void $ STA.splice 1 2 [ 0, 100 ] arr
      pure arr
  assert $ splicedReplaced == [ 1, 0, 100, 4, 5 ]

  log "toAssocArray should return all items in the array with the correct indices and values"

  assert $ all (\{ value: v, index: i } -> v == i + 1) $ ST.run do
    arr <- STA.thaw [ 1, 2, 3, 4, 5 ]
    STA.toAssocArray arr

  assert $ all (\{ value: v, index: i } -> v == (i + 1) * 10) $ ST.run do
    arr <- STA.thaw [ 10, 20, 30, 40, 50 ]
    STA.toAssocArray arr

nil :: Array Int
nil = []
