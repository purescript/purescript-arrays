module Test.Data.Array.Unsafe (testArrayUnsafe) where

import Prelude ((==), ($), bind, Unit)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Data.Array.Unsafe (init, last, tail, head)
import Test.Assert (assert, ASSERT)

testArrayUnsafe :: forall t.
        Eff
          ( console :: CONSOLE
          , assert :: ASSERT
          | t
          )
          Unit
testArrayUnsafe = do

  log "head should return the first item in an array"
  assert $ head [1, 2, 3] == 1
  assert $ head [1] == 1

  log "tail should return all but the first item in an array"
  assert $ tail [1, 2, 3] == [2, 3]

  log "last should return the last item of an array"
  assert $ last [1, 2, 3] == 3
  assert $ last [1] == 1

  log "init should return all but the last item of an array"
  assert $ init [1, 2, 3] == [1, 2]
