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
    , benchNubEq
    , benchUnion
    , benchIntersect
    , benchDifference
    ]


  where
  shortNats = Array.range 0 100
  longNats = Array.range 0 10000
  onlyEven x = if x `mod` 2 == 0 then Just x else Nothing
  mod3Eq x y = (x `mod` 3) == (y `mod` 3)

  benchMapMaybe = do
    log "mapMaybe"
    log "---------------"
    
    log $ "mapMaybe (" <> show (Array.length shortNats) <> ")"
    benchWith 1000 \_ -> Array.mapMaybe onlyEven shortNats

    log $ "mapMaybe (" <> show (Array.length longNats) <> ")"
    benchWith 100 \_ -> Array.mapMaybe onlyEven longNats

  benchNubEq = do
    log "nubEq"
    log "---------------"

    log $ "nubEq (" <> show (Array.length shortNats) <> ")"
    benchWith 1000 \_ -> Array.nubEq shortNats

    log $ "nubEq (" <> show (Array.length longNats) <> ")"
    benchWith 100 \_ -> Array.nubEq longNats

  benchUnion = do
    log "union"
    log "---------------"

    log $ "union (" <> show (Array.length shortNats) <> ")"
    benchWith 1000 \_ -> Array.union shortNats shortNats

    log $ "union (" <> show (Array.length longNats) <> ")"
    benchWith 100 \_ -> Array.union longNats longNats

  benchIntersect = do
    log "intersect"
    log "---------------"

    log $ "intersectBy (" <> show (Array.length shortNats) <> ")"
    benchWith 1000 \_ -> Array.intersect shortNats shortNats

    log $ "intersectBy (" <> show (Array.length longNats) <> ")"
    benchWith 100 \_ -> Array.intersect longNats longNats

  benchDifference = do
    log "difference"
    log "---------------"

    log $ "difference (" <> show (Array.length shortNats) <> ")"
    benchWith 1000 \_ -> Array.difference shortNats shortNats

    log $ "difference (" <> show (Array.length longNats) <> ")"
    benchWith 100 \_ -> Array.difference longNats longNats
