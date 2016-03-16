module Test.Main where

import Prelude (bind, Unit)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Test.Assert (ASSERT)
import Test.Data.Array (testArray)
import Test.Data.Array.ST (testArrayST)
import Test.Data.Array.Unsafe (testArrayUnsafe)

main :: forall t.
      Eff
        ( console :: CONSOLE
        , assert :: ASSERT
        | t
        )
        Unit
main = do
  testArray
  testArrayST
  testArrayUnsafe
