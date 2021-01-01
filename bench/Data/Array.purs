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
    , benchUnionBy
    , benchIntersectBy
    , benchDifference
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

  benchUnionBy = do
    log "unionBy"
    log "---------------"
    let shortNats = Array.range 0 100
        longNats = Array.range 0 10000
        mod3Eq x y = (x `mod` 3) == (y `mod` 3)

    log $ "unionBy (" <> show (Array.length shortNats) <> ")"
    benchWith 1000 \_ -> Array.unionBy mod3Eq shortNats shortNats

    log $ "unionBy (" <> show (Array.length longNats) <> ")"
    benchWith 100 \_ -> Array.unionBy mod3Eq longNats longNats

  benchIntersectBy = do
    log "intersectBy"
    log "---------------"
    let shortNats = Array.range 0 100
        longNats = Array.range 0 10000
        mod3Eq x y = (x `mod` 3) == (y `mod` 3)

    log $ "intersectBy (" <> show (Array.length shortNats) <> ")"
    benchWith 1000 \_ -> Array.intersectBy mod3Eq shortNats shortNats

    log $ "intersectBy (" <> show (Array.length longNats) <> ")"
    benchWith 100 \_ -> Array.intersectBy mod3Eq longNats longNats

  benchDifference = do
    log "difference"
    log "---------------"
    let shortNats = Array.range 0 100
        longNats = Array.range 0 10000
        mod3Eq x y = (x `mod` 3) == (y `mod` 3)

    log $ "difference (" <> show (Array.length shortNats) <> ")"
    benchWith 1000 \_ -> Array.difference shortNats shortNats

    log $ "difference (" <> show (Array.length longNats) <> ")"
    benchWith 100 \_ -> Array.difference longNats longNats
