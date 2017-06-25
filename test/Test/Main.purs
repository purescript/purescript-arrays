module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)

import Test.Assert (ASSERT)
import Test.Data.Array (testArray)
import Test.Data.Array.Partial (testArrayPartial)
import Test.Data.Array.ST (testArrayST)
import Test.Data.Array.ST.Partial (testArraySTPartial)

main :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
main = do
  testArray
  testArrayST
  testArrayPartial
  testArraySTPartial
