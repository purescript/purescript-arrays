module Test.Data.Array (testArray) where

import Data.Array
import Data.Int (Int(), fromNumber)
import Data.Maybe (Maybe(..))
import Debug.Trace (trace)
import Test.Common

testArray = do

  trace "test equality"
  assert $ [1] == [1]
  assert $ [1, 2, 3] == [1, 2, 3]

  trace "test inequality"
  assert $ [1] /= [2]
  assert $ [1, 2, 3] /= [1, 2, 2]

  trace "(!!) should return Just x when the index is within the bounds of the array"
  assert $ [1, 2, 3] !! (fromNumber 0) == (Just 1)
  assert $ [1, 2, 3] !! (fromNumber 1) == (Just 2)
  assert $ [1, 2, 3] !! (fromNumber 2) == (Just 3)

  trace "(!!) should return Nothing when the index is outside of the bounds of the array"
  assert $ [1, 2, 3] !! (fromNumber 6) == Nothing
  assert $ [1, 2, 3] !! (fromNumber (-1)) == Nothing

  trace "snoc should add an item to the end of an array"
  assert $ [1, 2, 3] `snoc` 4 == [1, 2, 3, 4]
  assert $ nil `snoc` 1 == [1]

  trace "singleton should construct an array with a single value"
  assert $ singleton 1 == [1]
  assert $ singleton "foo" == ["foo"]
  assert $ singleton nil == [[]]

  trace "head should return a Just-wrapped first value of a non-empty array"
  assert $ head ["foo", "bar"] == Just "foo"

  trace "head should return Nothing for an empty array"
  assert $ head nil == Nothing

  trace "last should return a Just-wrapped last value of a non-empty array"
  assert $ last ["foo", "bar"] == Just "bar"

  trace "last should return Nothing for an empty array"
  assert $ last nil == Nothing

  trace "tail should return a Just-wrapped array containing all the items in an array apart from the first for a non-empty array"
  assert $ tail ["foo", "bar", "baz"] == Just ["bar", "baz"]

  trace "tail should return Nothing for an empty array"
  assert $ tail nil == Nothing

  trace "init should return a Just-wrapped array containing all the items in an array apart from the first for a non-empty array"
  assert $ init ["foo", "bar", "baz"] == Just ["foo", "bar"]

  trace "init should return Nothing for an empty array"
  assert $ init nil == Nothing

  trace "null should return false for non-empty arrays"
  assert $ null [1] == false
  assert $ null [1, 2, 3] == false

  trace "null should return true for an empty array"
  assert $ null nil == true

  trace "length should return the number of items in an array"
  assert $ length nil == fromNumber 0
  assert $ length [1] == fromNumber 1
  assert $ length [1, 2, 3, 4, 5] == fromNumber 5

  trace "findIndex should return the index of an item that a predicate returns true for in an array"
  assert $ (findIndex (/= 1) [1, 2, 1]) == Just (fromNumber 1)
  assert $ (findIndex (== 3) [1, 2, 1]) == Nothing

  trace "findLastIndex should return the last index of an item in an array"
  assert $ (findLastIndex (/= 1) [2, 1, 2]) == Just (fromNumber 2)
  assert $ (findLastIndex (== 3) [2, 1, 2]) == Nothing

  trace "elemIndex should return the index of an item that a predicate returns true for in an array"
  assert $ (elemIndex 1 [1, 2, 1]) == Just (fromNumber 0)
  assert $ (elemIndex 4 [1, 2, 1]) == Nothing

  trace "elemLastIndex should return the last index of an item in an array"
  assert $ (elemLastIndex 1 [1, 2, 1]) == Just (fromNumber 2)
  assert $ (elemLastIndex 4 [1, 2, 1]) == Nothing

  trace "append should joint two arrays"
  assert $ (append [1, 2] [3, 4]) == [1, 2, 3, 4]
  assert $ (append [1] nil) == [1]
  assert $ (append nil nil) == nil

  trace "concat should join an array of arrays"
  assert $ (concat [[1, 2], [3, 4]]) == [1, 2, 3, 4]
  assert $ (concat [[1], nil]) == [1]
  assert $ (concat [nil, nil]) == nil

  trace "reverse should reverse the order of items in an array"
  assert $ (reverse [1, 2, 3]) == [3, 2, 1]
  assert $ (reverse nil) == nil

  trace "drop should remove the specified number of items from the front of an array"
  assert $ (drop (fromNumber 1) [1, 2, 3]) == [2, 3]
  assert $ (drop (fromNumber 2) [1, 2, 3]) == [3]
  assert $ (drop (fromNumber 1) nil) == nil

  trace "take should keep the specified number of items from the front of an array, discarding the rest"
  assert $ (take (fromNumber 1) [1, 2, 3]) == [1]
  assert $ (take (fromNumber 2) [1, 2, 3]) == [1, 2]
  assert $ (take (fromNumber 1) nil) == nil

  trace "insertAt should add an item at the specified index"
  assert $ (insertAt (fromNumber 0) 1 [2, 3]) == [1, 2, 3]
  assert $ (insertAt (fromNumber 1) 1 [2, 3]) == [2, 1, 3]

  trace "deleteAt should remove an item at the specified index"
  assert $ (deleteAt (fromNumber 0) (fromNumber 1) [1, 2, 3]) == [2, 3]
  assert $ (deleteAt (fromNumber 1) (fromNumber 1) [1, 2, 3]) == [1, 3]

  trace "updateAt should replace an item at the specified index"
  assert $ (updateAt (fromNumber 0) 9 [1, 2, 3]) == [9, 2, 3]
  assert $ (updateAt (fromNumber 1) 9 [1, 2, 3]) == [1, 9, 3]
  assert $ (updateAt (fromNumber 1) 9 nil) == nil

  trace "modifyAt should update an item at the specified index"
  assert $ (modifyAt (fromNumber 0) (+ 1) [1, 2, 3]) == [2, 2, 3]
  assert $ (modifyAt (fromNumber 1) (+ 1) [1, 2, 3]) == [1, 3, 3]
  assert $ (modifyAt (fromNumber 1) (+ 1) nil) == nil

  trace "delete should remove the first matching item from an array"
  assert $ delete 1 [1, 2, 1] == [2, 1]
  assert $ delete 2 [1, 2, 1] == [1, 1]

  trace "deleteBy should remove the first equality-relation-matching item from an array"
  assert $ deleteBy (/=) 2 [1, 2, 1] == [2, 1]
  assert $ deleteBy (/=) 1 [1, 2, 1] == [1, 1]

  trace "(\\) should return the difference between two lists"
  assert $ [1, 2, 3, 4, 3, 2, 1] \\ [1, 1, 2, 3] == [4, 3, 2]

  trace "intersect should return the intersection of two arrays"
  assert $ intersect [1, 2, 3, 4, 3, 2, 1] [1, 1, 2, 3] == [1, 2, 3, 3, 2, 1]

  trace "intersect should return the intersection of two arrays using the specified equivalence relation"
  assert $ intersectBy (\x y -> (x * 2) == y) [1, 2, 3] [2, 6] == [1, 3]

  trace "concatMap should be equivalent to (concat <<< map)"
  assert $ concatMap doubleAndOrig [1, 2, 3] == concat (map doubleAndOrig [1, 2, 3])

  trace "map should transform every item in an array"
  assert $ map (* 2) [1, 2, 3] == [2, 4, 6]

  trace "mapMaybe should transform every item in an array, throwing out Nothing values"
  assert $ mapMaybe (\x -> if x /= 0 then Just x else Nothing) [0, 1, 0, 0, 2, 3] == [1, 2, 3]

  trace "catMaybe should take an array of Maybe values and throw out Nothings"
  assert $ catMaybes [Nothing, Just 2, Nothing, Just 4] == [2, 4]

  trace "filter should remove items that don't match a predicate"
  assert $ filter odd (range 0 10) == [1, 3, 5, 7, 9]

  trace "range should create an inclusive array of integers for the specified start and end"
  assert $ range 0 5 == [0, 1, 2, 3, 4, 5]
  assert $ range 2 (-3) == [2, 1, 0, -1, -2, -3]

  -- trace "zipWith should use the specified function to zip two lists together"
  -- assert $ zipWith  [1, 2, 3] ["a", "b", "c"] == [Tuple 1 "a", Tuple 2 "b", Tuple 3 "c"]

  trace "nub should remove duplicate items from the list"
  assert $ nub [1, 2, 2, 3, 4, 1] == [1, 2, 3, 4]

  trace "nubBy should remove duplicate items from the list using a supplied predicate"
  let nubPred = \x y -> if odd x then false else x == y
  assert $ nubBy nubPred [1, 2, 2, 3, 3, 4, 4, 1] == [1, 2, 3, 3, 4, 1]

  trace "sort should reorder a list into ascending order based on the result of compare"
  assert $ sort [1, 3, 2, 5, 6, 4] == [1, 2, 3, 4, 5, 6]

  -- sortBy

  -- group
  -- group'
  -- groupBy

  -- span
  -- takeWhile
  -- dropWhile
  -- replicate

nil :: [Number]
nil = []

odd :: Number -> Boolean
odd n = (fromNumber n) `mod` (fromNumber 2) /= zero

doubleAndOrig :: Number -> [Number]
doubleAndOrig x = [x * 2, x]
