module Test.Data.Array (testArray) where

import Prelude

import Data.Array ((:), (\\), (!!))
import Data.Array as A
import Data.Array.NonEmpty as NEA
import Data.Const (Const(..))
import Data.Foldable (for_, foldMapDefaultR, class Foldable, all, traverse_)
import Data.Traversable (scanl, scanr)
import Data.Maybe (Maybe(..), isNothing, fromJust)
import Data.Ord.Down (Down(..))
import Data.Tuple (Tuple(..), fst)
import Data.Unfoldable (replicateA)
import Effect (Effect)
import Effect.Console (log)
import Partial.Unsafe (unsafePartial)
import Test.Assert (assert)
import Test.Data.UndefinedOr (defined, undefined)

testArray :: Effect Unit
testArray = do

  log "singleton should construct an array with a single value"
  assert $ A.singleton 1 == [1]
  assert $ A.singleton "foo" == ["foo"]
  assert $ A.singleton nil == [[]]

  log "range should create an inclusive array of integers for the specified start and end"
  assert $ (A.range 0 5) == [0, 1, 2, 3, 4, 5]
  assert $ (A.range 2 (-3)) == [2, 1, 0, -1, -2, -3]

  log "replicate should produce an array containg an item a specified number of times"
  assert $ A.replicate 3 true == [true, true, true]
  assert $ A.replicate 1 "foo" == ["foo"]
  assert $ A.replicate 0 "foo" == []
  assert $ A.replicate (-1) "foo" == []

  log "replicateA should perform the monadic action the correct number of times"
  assert $ replicateA 3 (Just 1) == Just [1, 1, 1]
  assert $ replicateA 1 (Just 1) == Just [1]
  assert $ replicateA 0 (Just 1) == Just []
  assert $ replicateA (-1) (Just 1) == Just []

  log "replicateA should be stack safe"
  for_ [1, 1000, 2000, 20000, 50000] \n -> do
    assert $ replicateA n (Just unit) == Just (A.replicate n unit :: Array Unit)

  -- some
  -- many

  log "null should return false for non-empty arrays"
  assert $ A.null [1] == false
  assert $ A.null [1, 2, 3] == false

  log "null should return true for an empty array"
  assert $ A.null nil == true

  log "length should return the number of items in an array"
  assert $ A.length nil == 0
  assert $ A.length [1] == 1
  assert $ A.length [1, 2, 3, 4, 5] == 5

  log "cons should add an item to the start of an array"
  assert $ 4 : [1, 2, 3] == [4, 1, 2, 3]
  assert $ 1 : nil == [1]

  log "snoc should add an item to the end of an array"
  assert $ [1, 2, 3] `A.snoc` 4 == [1, 2, 3, 4]
  assert $ nil `A.snoc` 1 == [1]

  log "insert should add an item at the appropriate place in a sorted array"
  assert $ A.insert 1.5 [1.0, 2.0, 3.0] == [1.0, 1.5, 2.0, 3.0]
  assert $ A.insert 4 [1, 2, 3] == [1, 2, 3, 4]
  assert $ A.insert 0 [1, 2, 3] == [0, 1, 2, 3]

  log "insertBy should add an item at the appropriate place in a sorted array using the specified comparison"
  assert $ A.insertBy (flip compare) 1.5 [1.0, 2.0, 3.0] == [1.0, 2.0, 3.0, 1.5]
  assert $ A.insertBy (flip compare) 4 [1, 2, 3] == [4, 1, 2, 3]
  assert $ A.insertBy (flip compare) 0 [1, 2, 3] == [1, 2, 3, 0]

  log "head should return a Just-wrapped first value of a non-empty array"
  assert $ A.head ["foo", "bar"] == Just "foo"

  log "head should return Nothing for an empty array"
  assert $ A.head nil == Nothing

  log "last should return a Just-wrapped last value of a non-empty array"
  assert $ A.last ["foo", "bar"] == Just "bar"

  log "last should return Nothing for an empty array"
  assert $ A.last nil == Nothing

  log "tail should return a Just-wrapped array containing all the items in an array apart from the first for a non-empty array"
  assert $ A.tail ["foo", "bar", "baz"] == Just ["bar", "baz"]

  log "tail should return Nothing for an empty array"
  assert $ A.tail nil == Nothing

  log "init should return a Just-wrapped array containing all the items in an array apart from the first for a non-empty array"
  assert $ A.init ["foo", "bar", "baz"] == Just ["foo", "bar"]

  log "init should return Nothing for an empty array"
  assert $ A.init nil == Nothing

  log "uncons should return nothing when used on an empty array"
  assert $ isNothing (A.uncons nil)

  log "uncons should split an array into a head and tail record when there is at least one item"
  let u1 = unsafePartial $ fromJust $ A.uncons [1]
  assert $ u1.head == 1
  assert $ u1.tail == []
  let u2 = unsafePartial $ fromJust $ A.uncons [1, 2, 3]
  assert $ u2.head == 1
  assert $ u2.tail == [2, 3]

  log "unsnoc should return nothing when used on an empty array"
  assert $ isNothing (A.unsnoc nil)

  log "unsnoc should split an array into an init and last record when there is at least one item"
  let u3 = unsafePartial $ fromJust $ A.unsnoc [1]
  assert $ u3.init == []
  assert $ u3.last == 1
  let u4 = unsafePartial $ fromJust $ A.unsnoc [1, 2, 3]
  assert $ u4.init == [1, 2]
  assert $ u4.last == 3

  log "(!!) should return Just x when the index is within the bounds of the array"
  assert $ [1, 2, 3] !! 0 == (Just 1)
  assert $ [1, 2, 3] !! 1 == (Just 2)
  assert $ [1, 2, 3] !! 2 == (Just 3)

  log "(!!) should return Nothing when the index is outside of the bounds of the array"
  assert $ [1, 2, 3] !! 6 == Nothing
  assert $ [1, 2, 3] !! (-1) == Nothing

  log "elem should return true if the array contains the given element at least once"
  assert $ (A.elem 1 [1, 2, 1]) == true
  assert $ (A.elem 4 [1, 2, 1]) == false

  log "notElem should return true if the array does not contain the given element"
  assert $ (A.notElem 1 [1, 2, 1]) == false
  assert $ (A.notElem 4 [1, 2, 1]) == true

  log "elemIndex should return the index of an item that a predicate returns true for in an array"
  assert $ (A.elemIndex 1 [1, 2, 1]) == Just 0
  assert $ (A.elemIndex 4 [1, 2, 1]) == Nothing

  log "elemLastIndex should return the last index of an item in an array"
  assert $ (A.elemLastIndex 1 [1, 2, 1]) == Just 2
  assert $ (A.elemLastIndex 4 [1, 2, 1]) == Nothing

  log "find should return the first element for which a predicate returns true in an array"
  assert $ (A.find (_ /= 1) [1, 2, 1]) == Just 2
  assert $ (A.find (_ == 3) [1, 2, 1]) == Nothing

  log "findMap should return the mapping of the first element that satisfies the given predicate"
  assert $ (A.findMap (\x -> if x > 3 then Just x else Nothing) [1, 2, 4]) == Just 4
  assert $ (A.findMap (\x -> if x > 3 then Just x else Nothing) [1, 2, 1]) == Nothing
  assert $ (A.findMap (\x -> if x > 3 then Just x else Nothing) [4, 1, 5]) == Just 4

  log "findIndex should return the index of an item that a predicate returns true for in an array"
  assert $ (A.findIndex (_ /= 1) [1, 2, 1]) == Just 1
  assert $ (A.findIndex (_ == 3) [1, 2, 1]) == Nothing

  log "findLastIndex should return the last index of an item in an array"
  assert $ (A.findLastIndex (_ /= 1) [2, 1, 2]) == Just 2
  assert $ (A.findLastIndex (_ == 3) [2, 1, 2]) == Nothing

  log "insertAt should add an item at the specified index"
  assert $ (A.insertAt 0 1 [2, 3]) == Just [1, 2, 3]
  assert $ (A.insertAt 1 1 [2, 3]) == Just [2, 1, 3]
  assert $ (A.insertAt 2 1 [2, 3]) == Just [2, 3, 1]

  log "insertAt should return Nothing if the index is out of A.range"
  assert $ (A.insertAt 2 1 nil) == Nothing

  log "deleteAt should remove an item at the specified index"
  assert $ (A.deleteAt 0 [1, 2, 3]) == Just [2, 3]
  assert $ (A.deleteAt 1 [1, 2, 3]) == Just [1, 3]

  log "deleteAt should return Nothing if the index is out of A.range"
  assert $ (A.deleteAt 1 nil) == Nothing

  log "updateAt should replace an item at the specified index"
  assert $ (A.updateAt 0 9 [1, 2, 3]) == Just [9, 2, 3]
  assert $ (A.updateAt 1 9 [1, 2, 3]) == Just [1, 9, 3]

  log "updateAt should return Nothing if the index is out of A.range"
  assert $ (A.updateAt 1 9 nil) == Nothing

  log "modifyAt should update an item at the specified index"
  assert $ (A.modifyAt 0 (_ + 1) [1, 2, 3]) == Just [2, 2, 3]
  assert $ (A.modifyAt 1 (_ + 1) [1, 2, 3]) == Just [1, 3, 3]

  log "modifyAt should return Nothing if the index is out of A.range"
  assert $ (A.modifyAt 1 (_ + 1) nil) == Nothing

  log "alterAt should update an item at the specified index when the function returns Just"
  assert $ (A.alterAt 0 (Just <<< (_ + 1)) [1, 2, 3]) == Just [2, 2, 3]
  assert $ (A.alterAt 1 (Just <<< (_ + 1)) [1, 2, 3]) == Just [1, 3, 3]

  log "alterAt should drop an item at the specified index when the function returns Nothing"
  assert $ (A.alterAt 0 (const Nothing) [1, 2, 3]) == Just [2, 3]
  assert $ (A.alterAt 1 (const Nothing) [1, 2, 3]) == Just [1, 3]

  log "alterAt should return Nothing if the index is out of A.range"
  assert $ (A.alterAt 1 (Just <<< (_ + 1)) nil) == Nothing

  log "intersperse should return the original array when given an array with zero or one elements"
  assert $ (A.intersperse " " []) == []
  assert $ (A.intersperse " " ["a"]) == ["a"]

  log "intersperse should insert the given element in-between each element in an array with two or more elements"
  assert $ (A.intersperse " " ["a", "b"]) == ["a", " ", "b"]
  assert $ (A.intersperse 0 [ 1, 2, 3, 4, 5 ]) == [ 1, 0, 2, 0, 3, 0, 4, 0, 5 ]

  log "reverse should reverse the order of items in an array"
  assert $ (A.reverse [1, 2, 3]) == [3, 2, 1]
  assert $ (A.reverse nil) == nil

  log "concat should join an array of arrays"
  assert $ (A.concat [[1, 2], [3, 4]]) == [1, 2, 3, 4]
  assert $ (A.concat [[1], nil]) == [1]
  assert $ (A.concat [nil, nil]) == nil

  log "concatMap should be equivalent to (concat <<< map)"
  assert $ A.concatMap doubleAndOrig [1, 2, 3] == A.concat (map doubleAndOrig [1, 2, 3])

  log "filter should remove items that don't match a predicate"
  assert $ A.filter odd (A.range 0 10) == [1, 3, 5, 7, 9]

  log "splitAt should split the array at the given number of elements"
  assert $ A.splitAt 2 ([] :: Array Int) == { before: [], after: [] }
  assert $ A.splitAt 3 [1, 2, 3, 4, 5] == { before: [1, 2, 3], after: [4, 5] }
  assert $ A.splitAt 1 [1, 2, 3] == { before: [1], after: [2, 3] }
  assert $ A.splitAt 3 [1, 2, 3] == { before: [1, 2, 3], after: [] }
  assert $ A.splitAt 4 [1, 2, 3] == { before: [1, 2, 3], after: [] }
  assert $ A.splitAt 0 [1, 2, 3] == { before: [], after: [1, 2, 3] }
  assert $ A.splitAt (-1) [1, 2, 3] == { before: [], after: [1, 2, 3] }

  log "filterA should remove items that don't match a predicate while using an applicative behaviour"
  assert $ A.filterA (Just <<< odd) (A.range 0 10) == Just [1, 3, 5, 7, 9]
  assert $ A.filterA (const Nothing) (A.range 0 10) == Nothing

  log "filterA should apply effects in the right order"
  assert $ A.filterA (Const <<< show) (A.range 1 5) == Const "12345"

  log "mapMaybe should transform every item in an array, throwing out Nothing values"
  assert $ A.mapMaybe (\x -> if x /= 0 then Just x else Nothing) [0, 1, 0, 0, 2, 3] == [1, 2, 3]

  log "catMaybe should take an array of Maybe values and throw out Nothings"
  assert $ A.catMaybes [Nothing, Just 2, Nothing, Just 4] == [2, 4]

  log "mapWithIndex applies a function with an index for every element"
  assert $ A.mapWithIndex (\i x -> x - i) [9,8,7,6,5] == [9,7,5,3,1]

  log "updateAtIndices changes the elements at specified indices"
  assert $ A.updateAtIndices
             [Tuple 0 false, Tuple 2 false, Tuple 8 false]
             [true,  true, true,  true] ==
             [false, true, false, true]

  log "modifyAtIndices modifies the elements at specified indices"
  assert $ A.modifyAtIndices [0, 2, 8] not [true,  true, true,  true] ==
                                           [false, true, false, true]

  log "transpose swaps rows and columns for a regular two-dimension array"
  assert $ A.transpose [[1,2,3], [4,5,6], [7,8,9]] ==
                       [[1,4,7], [2,5,8], [3,6,9]] 
  
  log "transpose skips elements when rows don't match"
  assert $ A.transpose [[10,11], [20], [30,31,32]] ==
                       [[10,20,30], [11,31], [32]]

  log "transpose [] == []"
  assert $ A.transpose [] == ([] :: Array (Array Int))

  log "transpose (singleton []) == []"
  assert $ A.transpose (A.singleton []) == ([] :: Array (Array Int))                                          

  log "scanl should return an array that stores the accumulated value at each step"
  assert $ A.scanl (+)  0 [1,2,3] == [1, 3, 6]
  assert $ A.scanl (-) 10 [1,2,3] == [9, 7, 4]

  log "scanl should return the same results as its Foldable counterpart"
  assert $ A.scanl (+)  0 [1,2,3] == scanl (+)  0 [1,2,3]
  assert $ A.scanl (-) 10 [1,2,3] == scanl (-) 10 [1,2,3]

  log "scanr should return an array that stores the accumulated value at each step"
  assert $ A.scanr (+) 0 [1,2,3] == [6,5,3]
  assert $ A.scanr (flip (-)) 10 [1,2,3] == [4,5,7]

  log "scanr should return the same results as its Foldable counterpart"
  assert $ A.scanr (+) 0 [1,2,3] == scanr (+) 0 [1,2,3]
  assert $ A.scanr (flip (-)) 10 [1,2,3] == scanr (flip (-)) 10 [1,2,3]

  log "sort should reorder a list into ascending order based on the result of compare"
  assert $ A.sort [1, 3, 2, 5, 6, 4] == [1, 2, 3, 4, 5, 6]
  assert $ A.sort [defined 1, undefined, defined 2] == [undefined, defined 1, defined 2]

  log "sortBy should reorder a list into ascending order based on the result of a comparison function"
  assert $ A.sortBy (flip compare) [1, 3, 2, 5, 6, 4] == [6, 5, 4, 3, 2, 1]

  log "sortBy should not reorder elements that are equal according to a comparison function"
  let s1 = map (Tuple "a") (A.range 1 100)
  assert $ A.sortBy (comparing fst) s1 == s1

  log "sortWith should reorder a list into ascending order based on the result of compare over a projection"
  assert $ A.sortWith identity [1, 3, 2, 5, 6, 4] == [1, 2, 3, 4, 5, 6]

  log "sortWith should not reorder elements that are equal according to a projection"
  let s2 = map (Tuple "a") (A.range 1 100)
  assert $ A.sortWith fst s2 == s2

  log "take should keep the specified number of items from the front of an array, discarding the rest"
  assert $ (A.take 1 [1, 2, 3]) == [1]
  assert $ (A.take 2 [1, 2, 3]) == [1, 2]
  assert $ (A.take 1 nil) == nil

  log "takeWhile should keep all values that match a predicate from the front of an array"
  assert $ (A.takeWhile (_ /= 2) [1, 2, 3]) == [1]
  assert $ (A.takeWhile (_ /= 3) [1, 2, 3]) == [1, 2]
  assert $ (A.takeWhile (_ /= 1) nil) == nil

  log "take should keep the specified number of items from the end of an array, discarding the rest"
  assert $ (A.takeEnd 1 [1, 2, 3]) == [3]
  assert $ (A.takeEnd 2 [1, 2, 3]) == [2, 3]
  assert $ (A.takeEnd 1 nil) == nil

  log "drop should remove the specified number of items from the front of an array"
  assert $ (A.drop 1 [1, 2, 3]) == [2, 3]
  assert $ (A.drop 2 [1, 2, 3]) == [3]
  assert $ (A.drop 1 nil) == nil

  log "dropWhile should remove all values that match a predicate from the front of an array"
  assert $ (A.dropWhile (_ /= 1) [1, 2, 3]) == [1, 2, 3]
  assert $ (A.dropWhile (_ /= 2) [1, 2, 3]) == [2, 3]
  assert $ (A.dropWhile (_ /= 1) nil) == nil

  log "drop should remove the specified number of items from the end of an array"
  assert $ (A.dropEnd 1 [1, 2, 3]) == [1, 2]
  assert $ (A.dropEnd 2 [1, 2, 3]) == [1]
  assert $ (A.dropEnd 1 nil) == nil

  log "take and drop should treat negative arguments as zero"
  assert $ (A.take (-2) [1, 2, 3]) == nil
  assert $ (A.drop (-2) [1, 2, 3]) == [1, 2, 3]

  log "span should split an array in two based on a predicate"
  let testSpan { p, input, init_, rest_ } = do
        let result = A.span p input
        assert $ result.init == init_
        assert $ result.rest == rest_

  let oneToSeven = [1, 2, 3, 4, 5, 6, 7]
  testSpan { p: (_ < 4), input: oneToSeven, init_: [1, 2, 3], rest_: [4, 5, 6, 7] }

  log "span with all elements satisfying the predicate"
  testSpan { p: const true, input: oneToSeven, init_: oneToSeven, rest_: [] }

  log "span with no elements satisfying the predicate"
  testSpan { p: const false, input: oneToSeven, init_: [], rest_: oneToSeven }

  log "span with large inputs: 10000"
  let testBigSpan n =
        testSpan { p: (_ < n), input: A.range 1 n, init_: A.range 1 (n-1), rest_: [n] }
  testBigSpan 10000

  log "span with large inputs: 40000"
  testBigSpan 40000

  log "span with large inputs: 100000"
  testBigSpan 100000

  log "group should group consecutive equal elements into arrays"
  assert $ A.group [1, 2, 2, 3, 3, 3, 1] == [nea [1], nea [2, 2], nea [3, 3, 3], nea [1]]

  log "groupAll should group equal elements into arrays"
  assert $ A.groupAll [1, 2, 2, 3, 3, 3, 1] == [nea [1, 1], nea [2, 2], nea [3, 3, 3]]

  log "groupBy should group consecutive equal elements into arrays based on an equivalence relation"
  assert $ A.groupBy (\x y -> odd x && odd y) [1, 1, 2, 2, 3, 3] == [nea [1, 1], nea [2], nea [2], nea [3, 3]]

  log "groupBy should be stable"
  assert $ A.groupBy (\_ _ -> true) [1, 2, 3] == [nea [1, 2, 3]]

  log "groupAllBy should group equal elements into arrays based on the result of a comparison function"
  assert $ A.groupAllBy (comparing Down) [1, 3, 2, 4, 3, 3] == [nea [4], nea [3, 3, 3], nea [2], nea [1]]

  log "groupAllBy should be stable"
  assert $ A.groupAllBy (\_ _ -> EQ) [1, 2, 3] == [nea [1, 2, 3]]

  log "nub should remove duplicate elements from the list, keeping the first occurence"
  assert $ A.nub [1, 2, 2, 3, 4, 1] == [1, 2, 3, 4]

  log "nub should preserve order"
  assert $ A.nub [1, 3, 4, 2, 2, 1] == [1, 3, 4, 2]

  log "nubEq should remove duplicate elements from the list, keeping the first occurence"
  assert $ A.nubEq [1, 2, 2, 3, 4, 1] == [1, 2, 3, 4]

  log "nubEq should preserve order"
  assert $ A.nubEq [1, 3, 4, 2, 2, 1] == [1, 3, 4, 2]

  log "nubBy should remove duplicate items from the list using a supplied predicate"
  assert $ A.nubBy compare [1, 3, 4, 2, 2, 1] == [1, 3, 4, 2]

  log "nubByEq should remove duplicate items from the list using a supplied predicate"
  let nubPred = \x y -> if odd x then false else x == y
  assert $ A.nubByEq nubPred [1, 2, 2, 3, 3, 4, 4, 1] == [1, 2, 3, 3, 4, 1]

  log "union should produce the union of two arrays"
  assert $ A.union [1, 2, 3] [2, 3, 4] == [1, 2, 3, 4]
  assert $ A.union [1, 1, 2, 3] [2, 3, 4] == [1, 1, 2, 3, 4]

  log "unionBy should produce the union of two arrays using the specified equality relation"
  assert $ A.unionBy (\_ y -> y < 5) [1, 2, 3] [2, 3, 4, 5, 6] == [1, 2, 3, 5, 6]

  log "delete should remove the first matching item from an array"
  assert $ A.delete 1 [1, 2, 1] == [2, 1]
  assert $ A.delete 2 [1, 2, 1] == [1, 1]

  log "deleteBy should remove the first equality-relation-matching item from an array"
  assert $ A.deleteBy (/=) 2 [1, 2, 1] == [2, 1]
  assert $ A.deleteBy (/=) 1 [1, 2, 1] == [1, 1]

  log "(\\\\) should return the difference between two lists"
  assert $ [1, 2, 3, 4, 3, 2, 1] \\ [1, 1, 2, 3] == [4, 3, 2]

  log "intersect should return the intersection of two arrays"
  assert $ A.intersect [1, 2, 3, 4, 3, 2, 1] [1, 1, 2, 3] == [1, 2, 3, 3, 2, 1]

  log "intersectBy should return the intersection of two arrays using the specified equivalence relation"
  assert $ A.intersectBy (\x y -> (x * 2) == y) [1, 2, 3] [2, 6] == [1, 3]

  log "zipWith should use the specified function to zip two lists together"
  assert $ A.zipWith (\x y -> [show x, y]) [1, 2, 3] ["a", "b", "c"] == [["1", "a"], ["2", "b"], ["3", "c"]]

  log "zipWithA should use the specified function to zip two lists together"
  assert $ A.zipWithA (\x y -> Just $ Tuple x y) [1, 2, 3] ["a", "b", "c"] == Just [Tuple 1 "a", Tuple 2 "b", Tuple 3 "c"]

  log "zip should use the specified function to zip two lists together"
  assert $ A.zip [1, 2, 3] ["a", "b", "c"] == [Tuple 1 "a", Tuple 2 "b", Tuple 3 "c"]

  log "unzip should deconstruct a list of tuples into a tuple of lists"
  assert $ A.unzip [Tuple 1 "a", Tuple 2 "b", Tuple 3 "c"] == Tuple [1, 2, 3] ["a", "b", "c"]

  log "any should return true if at least one array element satisfy the given predicate"
  assert $ not $ A.any (_ > 0) []
  assert $ A.any (_ > 0) [-1, 0, 1]
  assert $ not $ A.any (_ > 0) [-1, -2, -3]

  log "all should return true if all the array elements satisfy the given predicate"
  assert $ A.all (_ > 0) []
  assert $ A.all (_ > 0) [1, 2, 3]
  assert $ not $ A.all (_ > 0) [-1, -2, -3]

  log "foldM should perform a fold using a monadic step function"
  assert $ A.foldM (\x y -> Just (x + y)) 0 (A.range 1 10) == Just 55
  assert $ A.foldM (\_ _ -> Nothing) 0 (A.range 1 10) == Nothing

  log "fromFoldable"
  for_ [[], [1], [1,2], [1,2,3,4,5]] \xs -> do
    assert $ A.fromFoldable xs == xs

  log "fromFoldable is stack safe"
  for_ [1, 1000, 10000, 20000, 50000] \n -> do
    let elem = 0
    let arr = A.fromFoldable (Replicated n elem)
    assert $ A.length arr == n
    assert $ all (_ == elem) arr

  log "toUnfoldable"
  let toUnfoldableId xs = A.toUnfoldable xs == xs
  traverse_ (assert <<< toUnfoldableId)
    [ []
    , [1]
    , [1,2,3]
    , [2,3,1]
    , [4,0,0,1,25,36,458,5842,23757]
    ]

nea :: Array ~> NEA.NonEmptyArray
nea = unsafePartial fromJust <<< NEA.fromArray

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
