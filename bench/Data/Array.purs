module Bench.Data.Array where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Performance.Minibench (benchWith)

benchArray :: Effect Unit
benchArray = do
  log "mapMaybe"
  log "---------------"
  benchMapMaybe

  where

  benchMapMaybe = do
    let shortNats = Array.range 0 100
        longNats = Array.range 0 10000
        onlyEven x = if x `mod` 2 == 0 then Just x else Nothing

    log $ "mapMaybe (" <> show (Array.length shortNats) <> ")"
    benchWith 1000 \_ -> Array.mapMaybe onlyEven shortNats

    log $ "mapMaybe (" <> show (Array.length longNats) <> ")"
    benchWith 100 \_ -> Array.mapMaybe onlyEven longNats
