module Bench.Data.Array where

import Prelude

import Control.Monad.ST as ST
import Data.Array as Array
import Data.Array.ST as STA
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

  -- [from..to] >>= \x -> replicate x x
  mkArrayWithDuplicates from to = ST.run do
    arr <- STA.new
    ST.for from (to + 1) \n ->
      ST.for 0 n \_ ->
        void $ STA.push n arr
    STA.unsafeFreeze arr

  -- A.filter (between 45 55 <<< fst) $
  -- scanl (\t nxt -> bimap (_ + nxt) (_ `A.snoc` nxt) t) (Tuple 0 []) (A.range 4 200)
  -- > [(Tuple 49 [4,5,6,7,8,9,10])]
  shortNatsDup = Array.range 0 51 <> mkArrayWithDuplicates 4 10

  -- flip index 1 $ A.filter (between 450 550 <<< fst) $
  -- scanl (\t nxt -> bimap (_ + nxt) (_ `A.snoc` nxt) t) (Tuple 0 []) (A.range 11 300)
  -- > Tuple 506 [11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33]
  longNatsDup = Array.range 0 494 <> mkArrayWithDuplicates 11 33

  onlyEven x = if x `mod` 2 == 0 then Just x else Nothing
  mod3Eq x y = (x `mod` 3) == (y `mod` 3)

  benchMapMaybe = do
    log "mapMaybe"
    log "---------------"

    log $ "mapMaybe (" <> show (Array.length shortNatsDup) <> ")"
    benchWith 1000 \_ -> Array.mapMaybe onlyEven shortNatsDup

    log $ "mapMaybe (" <> show (Array.length longNatsDup) <> ")"
    benchWith 100 \_ -> Array.mapMaybe onlyEven longNatsDup

  benchNubEq = do
    log "nubEq"
    log "---------------"

    log $ "nubEq (" <> show (Array.length shortNatsDup) <> ")"
    benchWith 1000 \_ -> Array.nubEq shortNatsDup

    log $ "nubEq (" <> show (Array.length longNatsDup) <> ")"
    benchWith 100 \_ -> Array.nubEq longNatsDup

  benchUnion = do
    log "union"
    log "---------------"

    log $ "union (" <> show (Array.length shortNatsDup) <> ")"
    benchWith 1000 \_ -> Array.union shortNatsDup shortNatsDup

    log $ "union (" <> show (Array.length longNatsDup) <> ")"
    benchWith 100 \_ -> Array.union longNatsDup longNatsDup

  benchIntersect = do
    log "intersect"
    log "---------------"

    log $ "intersectBy (" <> show (Array.length shortNatsDup) <> ")"
    benchWith 1000 \_ -> Array.intersect shortNatsDup shortNatsDup

    log $ "intersectBy (" <> show (Array.length longNatsDup) <> ")"
    benchWith 100 \_ -> Array.intersect longNatsDup longNatsDup

  benchDifference = do
    log "difference"
    log "---------------"

    log $ "difference (" <> show (Array.length shortNatsDup) <> ")"
    benchWith 1000 \_ -> Array.difference shortNatsDup shortNatsDup

    log $ "difference (" <> show (Array.length longNatsDup) <> ")"
    benchWith 100 \_ -> Array.difference longNatsDup longNatsDup
