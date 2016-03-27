module Test.Data.Array (testArray) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)

import Data.Array (range, foldM, unzip, zip, zipWithA, zipWith, intersectBy, intersect, (\\), deleteBy, delete, unionBy, union, nubBy, nub, groupBy, group', group, span, dropWhile, drop, takeWhile, take, sortBy, sort, catMaybes, mapMaybe, filterM, filter, concat, concatMap, reverse, alterAt, modifyAt, updateAt, deleteAt, insertAt, findLastIndex, findIndex, elemLastIndex, elemIndex, (!!), uncons, init, tail, last, head, insertBy, insert, snoc, (:), length, null, replicate, replicateM, singleton, fromFoldable)
import Data.Foldable (for_, foldMapDefaultR, class Foldable, all)
import Data.Maybe (Maybe(..), isNothing, fromJust)
import Data.Tuple (Tuple(..))

import Partial.Unsafe (unsafePartial)

import Test.Assert (assert, ASSERT)

testArray :: forall eff. Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
testArray = do

  log "singleton should construct an array with a single value"
  assert $ singleton 1 == [1]
  assert $ singleton "foo" == ["foo"]
  assert $ singleton nil == [[]]

  log "range should create an inclusive array of integers for the specified start and end"
  assert $ (range 0 5) == [0, 1, 2, 3, 4, 5]
  assert $ (range 2 (-3)) == [2, 1, 0, -1, -2, -3]

  log "replicate should produce an array containg an item a specified number of times"
  assert $ replicate 3 true == [true, true, true]
  assert $ replicate 1 "foo" == ["foo"]
  assert $ replicate 0 "foo" == []
  assert $ replicate (-1) "foo" == []

  log "replicateM should perform the monadic action the correct number of times"
  assert $ replicateM 3 (Just 1) == Just [1, 1, 1]
  assert $ replicateM 1 (Just 1) == Just [1]
  assert $ replicateM 0 (Just 1) == Just []
  assert $ replicateM (-1) (Just 1) == Just []

  log "replicateM should be stack safe"
  for_ [1, 1000, 2000, 20000, 50000] \n -> do
    assert $ replicateM n (Just unit) == Just (replicate n unit)

  -- some
  -- many

  log "null should return false for non-empty arrays"
  assert $ null [1] == false
  assert $ null [1, 2, 3] == false

  log "null should return true for an empty array"
  assert $ null nil == true

  log "length should return the number of items in an array"
  assert $ length nil == 0
  assert $ length [1] == 1
  assert $ length [1, 2, 3, 4, 5] == 5

  log "cons should add an item to the start of an array"
  assert $ 4 : [1, 2, 3] == [4, 1, 2, 3]
  assert $ 1 : nil == [1]

  log "snoc should add an item to the end of an array"
  assert $ [1, 2, 3] `snoc` 4 == [1, 2, 3, 4]
  assert $ nil `snoc` 1 == [1]

  log "insert should add an item at the appropriate place in a sorted array"
  assert $ insert 1.5 [1.0, 2.0, 3.0] == [1.0, 1.5, 2.0, 3.0]
  assert $ insert 4 [1, 2, 3] == [1, 2, 3, 4]
  assert $ insert 0 [1, 2, 3] == [0, 1, 2, 3]

  log "insertBy should add an item at the appropriate place in a sorted array using the specified comparison"
  assert $ insertBy (flip compare) 1.5 [1.0, 2.0, 3.0] == [1.0, 2.0, 3.0, 1.5]
  assert $ insertBy (flip compare) 4 [1, 2, 3] == [4, 1, 2, 3]
  assert $ insertBy (flip compare) 0 [1, 2, 3] == [1, 2, 3, 0]

  log "head should return a Just-wrapped first value of a non-empty array"
  assert $ head ["foo", "bar"] == Just "foo"

  log "head should return Nothing for an empty array"
  assert $ head nil == Nothing

  log "last should return a Just-wrapped last value of a non-empty array"
  assert $ last ["foo", "bar"] == Just "bar"

  log "last should return Nothing for an empty array"
  assert $ last nil == Nothing

  log "tail should return a Just-wrapped array containing all the items in an array apart from the first for a non-empty array"
  assert $ tail ["foo", "bar", "baz"] == Just ["bar", "baz"]

  log "tail should return Nothing for an empty array"
  assert $ tail nil == Nothing

  log "init should return a Just-wrapped array containing all the items in an array apart from the first for a non-empty array"
  assert $ init ["foo", "bar", "baz"] == Just ["foo", "bar"]

  log "init should return Nothing for an empty array"
  assert $ init nil == Nothing

  log "uncons should return nothing when used on an empty array"
  assert $ isNothing (uncons nil)

  log "uncons should split an array into a head and tail record when there is at least one item"
  let u1 = uncons [1]
  assert $ (unsafePartial $ fromJust u1).head == 1
  assert $ (unsafePartial $ fromJust u1).tail == []
  let u2 = uncons [1, 2, 3]
  assert $ (unsafePartial $ fromJust u2).head == 1
  assert $ (unsafePartial $ fromJust u2).tail == [2, 3]

  log "(!!) should return Just x when the index is within the bounds of the array"
  assert $ [1, 2, 3] !! 0 == (Just 1)
  assert $ [1, 2, 3] !! 1 == (Just 2)
  assert $ [1, 2, 3] !! 2 == (Just 3)

  log "(!!) should return Nothing when the index is outside of the bounds of the array"
  assert $ [1, 2, 3] !! 6 == Nothing
  assert $ [1, 2, 3] !! (-1) == Nothing

  log "elemIndex should return the index of an item that a predicate returns true for in an array"
  assert $ (elemIndex 1 [1, 2, 1]) == Just 0
  assert $ (elemIndex 4 [1, 2, 1]) == Nothing

  log "elemLastIndex should return the last index of an item in an array"
  assert $ (elemLastIndex 1 [1, 2, 1]) == Just 2
  assert $ (elemLastIndex 4 [1, 2, 1]) == Nothing

  log "findIndex should return the index of an item that a predicate returns true for in an array"
  assert $ (findIndex (_ /= 1) [1, 2, 1]) == Just 1
  assert $ (findIndex (_ == 3) [1, 2, 1]) == Nothing

  log "findLastIndex should return the last index of an item in an array"
  assert $ (findLastIndex (_ /= 1) [2, 1, 2]) == Just 2
  assert $ (findLastIndex (_ == 3) [2, 1, 2]) == Nothing

  log "insertAt should add an item at the specified index"
  assert $ (insertAt 0 1 [2, 3]) == Just [1, 2, 3]
  assert $ (insertAt 1 1 [2, 3]) == Just [2, 1, 3]
  assert $ (insertAt 2 1 [2, 3]) == Just [2, 3, 1]

  log "insertAt should return Nothing if the index is out of range"
  assert $ (insertAt 2 1 nil) == Nothing

  log "deleteAt should remove an item at the specified index"
  assert $ (deleteAt 0 [1, 2, 3]) == Just [2, 3]
  assert $ (deleteAt 1 [1, 2, 3]) == Just [1, 3]

  log "deleteAt should return Nothing if the index is out of range"
  assert $ (deleteAt 1 nil) == Nothing

  log "updateAt should replace an item at the specified index"
  assert $ (updateAt 0 9 [1, 2, 3]) == Just [9, 2, 3]
  assert $ (updateAt 1 9 [1, 2, 3]) == Just [1, 9, 3]

  log "updateAt should return Nothing if the index is out of range"
  assert $ (updateAt 1 9 nil) == Nothing

  log "modifyAt should update an item at the specified index"
  assert $ (modifyAt 0 (_ + 1) [1, 2, 3]) == Just [2, 2, 3]
  assert $ (modifyAt 1 (_ + 1) [1, 2, 3]) == Just [1, 3, 3]

  log "modifyAt should return Nothing if the index is out of range"
  assert $ (modifyAt 1 (_ + 1) nil) == Nothing

  log "alterAt should update an item at the specified index when the function returns Just"
  assert $ (alterAt 0 (Just <<< (+ 1)) [1, 2, 3]) == Just [2, 2, 3]
  assert $ (alterAt 1 (Just <<< (+ 1)) [1, 2, 3]) == Just [1, 3, 3]

  log "alterAt should drop an item at the specified index when the function returns Nothing"
  assert $ (alterAt 0 (const Nothing) [1, 2, 3]) == Just [2, 3]
  assert $ (alterAt 1 (const Nothing) [1, 2, 3]) == Just [1, 3]

  log "alterAt should return Nothing if the index is out of range"
  assert $ (alterAt 1 (Just <<< (+ 1)) nil) == Nothing

  log "reverse should reverse the order of items in an array"
  assert $ (reverse [1, 2, 3]) == [3, 2, 1]
  assert $ (reverse nil) == nil

  log "concat should join an array of arrays"
  assert $ (concat [[1, 2], [3, 4]]) == [1, 2, 3, 4]
  assert $ (concat [[1], nil]) == [1]
  assert $ (concat [nil, nil]) == nil

  log "concatMap should be equivalent to (concat <<< map)"
  assert $ concatMap doubleAndOrig [1, 2, 3] == concat (map doubleAndOrig [1, 2, 3])

  log "filter should remove items that don't match a predicate"
  assert $ filter odd (range 0 10) == [1, 3, 5, 7, 9]

  log "filterM should remove items that don't match a predicate while using a monadic behaviour"
  assert $ filterM (Just <<< odd) (range 0 10) == Just [1, 3, 5, 7, 9]
  assert $ filterM (const Nothing) (range 0 10) == Nothing

  log "mapMaybe should transform every item in an array, throwing out Nothing values"
  assert $ mapMaybe (\x -> if x /= 0 then Just x else Nothing) [0, 1, 0, 0, 2, 3] == [1, 2, 3]

  log "catMaybe should take an array of Maybe values and throw out Nothings"
  assert $ catMaybes [Nothing, Just 2, Nothing, Just 4] == [2, 4]

  log "sort should reorder a list into ascending order based on the result of compare"
  assert $ sort [1, 3, 2, 5, 6, 4] == [1, 2, 3, 4, 5, 6]

  log "sortBy should reorder a list into ascending order based on the result of a comparison function"
  assert $ sortBy (flip compare) [1, 3, 2, 5, 6, 4] == [6, 5, 4, 3, 2, 1]

  log "take should keep the specified number of items from the front of an array, discarding the rest"
  assert $ (take 1 [1, 2, 3]) == [1]
  assert $ (take 2 [1, 2, 3]) == [1, 2]
  assert $ (take 1 nil) == nil

  log "takeWhile should keep all values that match a predicate from the front of an array"
  assert $ (takeWhile (_ /= 2) [1, 2, 3]) == [1]
  assert $ (takeWhile (_ /= 3) [1, 2, 3]) == [1, 2]
  assert $ (takeWhile (_ /= 1) nil) == nil

  log "drop should remove the specified number of items from the front of an array"
  assert $ (drop 1 [1, 2, 3]) == [2, 3]
  assert $ (drop 2 [1, 2, 3]) == [3]
  assert $ (drop 1 nil) == nil

  log "dropWhile should remove all values that match a predicate from the front of an array"
  assert $ (dropWhile (_ /= 1) [1, 2, 3]) == [1, 2, 3]
  assert $ (dropWhile (_ /= 2) [1, 2, 3]) == [2, 3]
  assert $ (dropWhile (_ /= 1) nil) == nil

  log "take and drop should treat negative arguments as zero"
  assert $ (take (-2) [1, 2, 3]) == nil
  assert $ (drop (-2) [1, 2, 3]) == [1, 2, 3]

  log "span should split an array in two based on a predicate"
  let spanResult = span (_ < 4) [1, 2, 3, 4, 5, 6, 7]
  assert $ spanResult.init == [1, 2, 3]
  assert $ spanResult.rest == [4, 5, 6, 7]

  log "group should group consecutive equal elements into arrays"
  assert $ group [1, 2, 2, 3, 3, 3, 1] == [[1], [2, 2], [3, 3, 3], [1]]

  log "group' should sort then group consecutive equal elements into arrays"
  assert $ group' [1, 2, 2, 3, 3, 3, 1] == [[1, 1], [2, 2], [3, 3, 3]]

  log "groupBy should group consecutive equal elements into arrays based on an equivalence relation"
  assert $ groupBy (\x y -> odd x && odd y) [1, 1, 2, 2, 3, 3] == [[1, 1], [2], [2], [3, 3]]

  log "nub should remove duplicate elements from the list, keeping the first occurence"
  assert $ nub [1, 2, 2, 3, 4, 1] == [1, 2, 3, 4]

  log "nubBy should remove duplicate items from the list using a supplied predicate"
  let nubPred = \x y -> if odd x then false else x == y
  assert $ nubBy nubPred [1, 2, 2, 3, 3, 4, 4, 1] == [1, 2, 3, 3, 4, 1]

  log "union should produce the union of two arrays"
  assert $ union [1, 2, 3] [2, 3, 4] == [1, 2, 3, 4]
  assert $ union [1, 1, 2, 3] [2, 3, 4] == [1, 1, 2, 3, 4]

  log "unionBy should produce the union of two arrays using the specified equality relation"
  assert $ unionBy (\_ y -> y < 5) [1, 2, 3] [2, 3, 4, 5, 6] == [1, 2, 3, 5, 6]

  log "delete should remove the first matching item from an array"
  assert $ delete 1 [1, 2, 1] == [2, 1]
  assert $ delete 2 [1, 2, 1] == [1, 1]

  log "deleteBy should remove the first equality-relation-matching item from an array"
  assert $ deleteBy (/=) 2 [1, 2, 1] == [2, 1]
  assert $ deleteBy (/=) 1 [1, 2, 1] == [1, 1]

  log "(\\\\) should return the difference between two lists"
  assert $ [1, 2, 3, 4, 3, 2, 1] \\ [1, 1, 2, 3] == [4, 3, 2]

  log "intersect should return the intersection of two arrays"
  assert $ intersect [1, 2, 3, 4, 3, 2, 1] [1, 1, 2, 3] == [1, 2, 3, 3, 2, 1]

  log "intersectBy should return the intersection of two arrays using the specified equivalence relation"
  assert $ intersectBy (\x y -> (x * 2) == y) [1, 2, 3] [2, 6] == [1, 3]

  log "zipWith should use the specified function to zip two lists together"
  assert $ zipWith (\x y -> [show x, y]) [1, 2, 3] ["a", "b", "c"] == [["1", "a"], ["2", "b"], ["3", "c"]]

  log "zipWithA should use the specified function to zip two lists together"
  assert $ zipWithA (\x y -> Just $ Tuple x y) [1, 2, 3] ["a", "b", "c"] == Just [Tuple 1 "a", Tuple 2 "b", Tuple 3 "c"]

  log "zip should use the specified function to zip two lists together"
  assert $ zip [1, 2, 3] ["a", "b", "c"] == [Tuple 1 "a", Tuple 2 "b", Tuple 3 "c"]

  log "unzip should deconstruct a list of tuples into a tuple of lists"
  assert $ unzip [Tuple 1 "a", Tuple 2 "b", Tuple 3 "c"] == Tuple [1, 2, 3] ["a", "b", "c"]

  log "foldM should perform a fold using a monadic step function"
  assert $ foldM (\x y -> Just (x + y)) 0 (range 1 10) == Just 55
  assert $ foldM (\_ _ -> Nothing) 0 (range 1 10) == Nothing

  log "fromFoldable"
  for_ [[], [1], [1,2], [1,2,3,4,5]] \xs -> do
    assert $ fromFoldable xs == xs

  log "fromFoldable is stack safe"
  for_ [1, 1000, 10000, 20000, 50000] \n -> do
    let elem = 0
    let arr = fromFoldable (Replicated n elem)
    assert $ length arr == n
    assert $ all (_ == elem) arr

nil :: Array Int
nil = []

odd :: Int -> Boolean
odd n = n `mod` 2 /= zero

doubleAndOrig :: Int -> Array Int
doubleAndOrig x = [x * 2, x]

data Replicated a = Replicated Int a

instance foldableReplicated :: Foldable Replicated where
  foldr f z (Replicated n x) = applyN n (f x) z
  foldl f z (Replicated n x) = applyN n (flip f x) z
  foldMap = foldMapDefaultR

applyN :: forall a. Int -> (a -> a) -> a -> a
applyN n f x
  | n <= 0    = x
  | otherwise = applyN (n - 1) f (f x)
