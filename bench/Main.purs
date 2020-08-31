module Bench.Main where

import Prelude

import Bench.Data.Array (benchArray)
import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  log "Array"
  log "==="
  benchArray
