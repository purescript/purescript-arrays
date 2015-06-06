module Test.Main where

import Prelude
import Test.Data.Array
import Test.Data.Array.ST
import Test.Data.Array.Unsafe

main = do
  testArray
  testArrayST
  testArrayUnsafe
