module Test.Main where

import Prelude

import Effect (Effect)
import Test.Data.Array (testArray)
import Test.Data.Array.NonEmpty (testNonEmptyArray)
import Test.Data.Array.Partial (testArrayPartial)
import Test.Data.Array.ST (testArrayST)
import Test.Data.Array.ST.Partial (testArraySTPartial)

main :: Effect Unit
main = do
  testArray
  testArrayST
  testArrayPartial
  testArraySTPartial
  testNonEmptyArray
