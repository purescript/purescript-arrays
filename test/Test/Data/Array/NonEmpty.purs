module Test.Data.Array.NonEmpty (testNonEmptyArray) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Array.NonEmpty as NEA
import Data.Foldable (sum)
import Data.Maybe (Maybe(..), fromJust)
import Data.Monoid.Additive (Additive(..))
import Data.Semigroup.Foldable (foldMap1)
import Data.Semigroup.Traversable (traverse1)
import Partial.Unsafe (unsafePartial)
import Test.Assert (ASSERT, assert)

testNonEmptyArray :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
testNonEmptyArray = do
  log "singleton should construct an array with a single value"
  assert $ NEA.toArray (NEA.singleton 1) == [1]
  assert $ NEA.toArray (NEA.singleton "foo") == ["foo"]

  log "range should create an inclusive array of integers for the specified start and end"
  assert $ NEA.toArray (unsafePartial fromJust (NEA.range 0 5))
           == [0, 1, 2, 3, 4, 5]
  assert $ NEA.toArray (unsafePartial fromJust (NEA.range 2 (-3)))
           == [2, 1, 0, -1, -2, -3]

  log "replicate should produce an array containg an item a specified number of times"
  assert $ NEA.toArray (NEA.replicate 3 true) == [true, true, true]
  assert $ NEA.toArray (NEA.replicate 1 "foo") == ["foo"]
  assert $ NEA.toArray (NEA.replicate 0 "foo") == ["foo"]
  assert $ NEA.toArray (NEA.replicate (-1) "foo") == ["foo"]

  log "length should return the number of items in an array"
  assert $ NEA.length (NEA.singleton 1) == 1
  assert $ NEA.length (unsafePartial fromJust (NEA.fromArray [1, 2, 3, 4, 5]))
           == 5

  log "foldl should work"
  -- test through sum
  assert $ sum (unsafePartial fromJust (NEA.fromArray [1, 2, 3, 4])) == 10

  log "foldMap1 should work"
  assert $ foldMap1 Additive (unsafePartial fromJust (NEA.fromArray [1, 2, 3, 4])) == Additive 10
  
  log "traverse1 should work"
  assert $ traverse1 Just (unsafePartial fromJust (NEA.fromArray [1, 2, 3, 4])) == NEA.fromArray [1, 2, 3, 4]
