module Bench.Data.Array where

import Prelude

import Data.Array as Array
import Data.Traversable (sequence_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Performance.Minibench (benchWith)

benchArray :: Effect Unit
benchArray = do
  sequence_ $ Array.intersperse (log "")
    [ benchMapMaybe
    , benchNubBy
    ]


  where

  benchMapMaybe = do
    log "mapMaybe"
    log "---------------"
    let shortNats = Array.range 0 100
        longNats = Array.range 0 10000
        onlyEven x = if x `mod` 2 == 0 then Just x else Nothing

    log $ "mapMaybe (" <> show (Array.length shortNats) <> ")"
    benchWith 1000 \_ -> Array.mapMaybe onlyEven shortNats

    log $ "mapMaybe (" <> show (Array.length longNats) <> ")"
    benchWith 100 \_ -> Array.mapMaybe onlyEven longNats

  benchNubBy = do
    log "nubBy"
    log "---------------"
    let shortNats = Array.range 0 100
        longNats = Array.range 0 10000
        mod3Cmp x y = compare (x `mod` 3) (y `mod` 3)

    log $ "nubBy (" <> show (Array.length shortNats) <> ")"
    benchWith 1000 \_ -> Array.nubBy mod3Cmp shortNats

    log $ "nubBy (" <> show (Array.length longNats) <> ")"
    benchWith 100 \_ -> Array.nubBy mod3Cmp longNats
