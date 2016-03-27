module Test.Main where

import Prelude (bind, Unit)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)

import Test.Assert (ASSERT)
import Test.Data.Array (testArray)
import Test.Data.Array.Partial (testArrayPartial)
import Test.Data.Array.ST (testArrayST)

main :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
main = do
  testArray
  testArrayST
  testArrayPartial
