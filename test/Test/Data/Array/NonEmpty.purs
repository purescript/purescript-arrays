module Test.Data.Array.NonEmpty (testNonEmptyArray) where

import Prelude

import Data.Array as A
import Data.Array.NonEmpty as NEA
import Data.Const (Const(..))
import Data.Foldable (for_, sum, traverse_)
import Data.Traversable (scanl, scanr)
import Data.Maybe (Maybe(..), fromJust)
import Data.Monoid.Additive (Additive(..))
import Data.NonEmpty ((:|))
import Data.Ord.Down (Down(..))
import Data.Semigroup.Foldable (foldMap1, foldr1, foldl1)
import Data.Semigroup.Traversable (traverse1)
import Data.Tuple (Tuple(..))
import Data.Unfoldable1 as U1
import Effect (Effect)
import Effect.Console (log)
import Partial.Unsafe (unsafePartial)
import Test.Assert (assert)
import Test.Data.UndefinedOr (defined, undefined)

testNonEmptyArray :: Effect Unit
testNonEmptyArray = do
  let fromArray :: forall a. Array a -> NEA.NonEmptyArray a
      fromArray = unsafePartial fromJust <<< NEA.fromArray

      nea :: forall a. Array a -> NEA.NonEmptyArray a
      nea = fromArray

  log "singleton should construct an array with a single value"
  assert $ NEA.toArray (NEA.singleton 1) == [1]
  assert $ NEA.toArray (NEA.singleton "foo") == ["foo"]

  log "range should create an inclusive array of integers for the specified start and end"
  assert $ NEA.toArray (NEA.range 0 5) == [0, 1, 2, 3, 4, 5]
  assert $ NEA.toArray (NEA.range 2 (-3)) == [2, 1, 0, -1, -2, -3]
  assert $ NEA.toArray (NEA.range 0 0) == [0]

  log "replicate should produce an array containg an item a specified number of times"
  assert $ NEA.toArray (NEA.replicate 3 true) == [true, true, true]
  assert $ NEA.toArray (NEA.replicate 1 "foo") == ["foo"]
  assert $ NEA.toArray (NEA.replicate 0 "foo") == ["foo"]
  assert $ NEA.toArray (NEA.replicate (-1) "foo") == ["foo"]

  log "length should return the number of items in an array"
  assert $ NEA.length (NEA.singleton 1) == 1
  assert $ NEA.length (fromArray [1, 2, 3, 4, 5]) == 5

  log "cons should add an item to the start of an array"
  assert $ NEA.cons 4 (fromArray [1, 2, 3]) == fromArray [4, 1, 2, 3]
  assert $ NEA.cons' 4 [1, 2, 3] == fromArray [4, 1, 2, 3]

  log "snoc should add an item to the end of an array"
  assert $ fromArray [1, 2, 3] `NEA.snoc` 4 == fromArray [1, 2, 3, 4]
  assert $ [1, 2, 3] `NEA.snoc'` 4 == fromArray [1, 2, 3, 4]

  log "insert should add an item at the appropriate place in a sorted array"
  assert $ NEA.insert 1.5 (fromArray [1.0, 2.0, 3.0])
           == fromArray [1.0, 1.5, 2.0, 3.0]
  assert $ NEA.insert 4 (fromArray [1, 2, 3]) == fromArray [1, 2, 3, 4]

  log "insertBy should add an item at the appropriate place in a sorted array using the specified comparison"
  assert $ NEA.insertBy (flip compare) 1.5 (fromArray [1.0, 2.0, 3.0])
           == fromArray [1.0, 2.0, 3.0, 1.5]

  log "head should return the first value of a non-empty array"
  assert $ NEA.head (fromArray ["foo", "bar"]) == "foo"

  log "last should return the last value of a non-empty array"
  assert $ NEA.last (fromArray ["foo", "bar"]) == "bar"

  log "tail should return an array containing all the items in an array apart from the first for a non-empty array"
  assert $ NEA.tail (fromArray ["foo", "bar", "baz"]) == ["bar", "baz"]

  log "init should return an array containing all the items in an array apart from the first for a non-empty array"
  assert $ NEA.init (fromArray ["foo", "bar", "baz"]) == ["foo", "bar"]

  log "uncons should split an array into a head and tail record"
  let u1 = NEA.uncons $ NEA.singleton 1
  assert $ u1.head == 1
  assert $ u1.tail == []
  let u2 = NEA.uncons $ fromArray [1, 2, 3]
  assert $ u2.head == 1
  assert $ u2.tail == [2, 3]

  log "unsnoc should split an array into an init and last record"
  let u3 = NEA.unsnoc $ NEA.singleton 1
  assert $ u3.init == []
  assert $ u3.last == 1
  let u4 = NEA.unsnoc $ fromArray [1, 2, 3]
  assert $ u4.init == [1, 2]
  assert $ u4.last == 3

  log "index should return Just x when the index is within the bounds of the array"
  assert $ NEA.index (fromArray [1, 2, 3]) 0 == Just 1
  assert $ NEA.index (fromArray [1, 2, 3]) 1 == Just 2
  assert $ NEA.index (fromArray [1, 2, 3]) 2 == Just 3

  log "index should return Nothing when the index is outside of the bounds of the array"
  assert $ NEA.index (fromArray [1, 2, 3]) 6 == Nothing
  assert $ NEA.index (fromArray [1, 2, 3]) (-1) == Nothing

  log "elem should return true if the array contains the given element at least once"
  assert $ NEA.elem 1 (fromArray [1, 2, 1]) == true
  assert $ NEA.elem 4 (fromArray [1, 2, 1]) == false

  log "notElem should return true if the array does not contain the given element"
  assert $ NEA.notElem 1 (fromArray [1, 2, 1]) == false
  assert $ NEA.notElem 4 (fromArray [1, 2, 1]) == true

  log "elemIndex should return the index of an item that a predicate returns true for in an array"
  assert $ NEA.elemIndex 1 (fromArray [1, 2, 1]) == Just 0
  assert $ NEA.elemIndex 4 (fromArray [1, 2, 1]) == Nothing

  log "elemLastIndex should return the last index of an item in an array"
  assert $ NEA.elemLastIndex 1 (fromArray [1, 2, 1]) == Just 2
  assert $ NEA.elemLastIndex 4 (fromArray [1, 2, 1]) == Nothing

  log "find should return the first element for which a predicate returns true in an array"
  assert $ NEA.find (_ == 1) (fromArray [1, 2, 1]) == Just 1
  assert $ NEA.find (_ == 3) (fromArray [1, 2, 1]) == Nothing

  log "findMap should return the mapping of the first element that satisfies the given predicate"
  assert $ NEA.findMap (\x -> if x > 3 then Just x else Nothing) (fromArray [1, 2, 4]) == Just 4
  assert $ NEA.findMap (\x -> if x > 3 then Just x else Nothing) (fromArray [1, 2, 1]) == Nothing
  assert $ NEA.findMap (\x -> if x > 3 then Just x else Nothing) (fromArray [4, 1, 5]) == Just 4

  log "findIndex should return the index of an item that a predicate returns true for in an array"
  assert $ (NEA.findIndex (_ /= 1) (fromArray [1, 2, 1])) == Just 1
  assert $ (NEA.findIndex (_ == 3) (fromArray [1, 2, 1])) == Nothing

  log "findLastIndex should return the last index of an item in an array"
  assert $ (NEA.findLastIndex (_ /= 1) (fromArray [2, 1, 2])) == Just 2
  assert $ (NEA.findLastIndex (_ == 3) (fromArray [2, 1, 2])) == Nothing

  log "insertAt should add an item at the specified index"
  assert $ NEA.insertAt 0 1 (fromArray [2, 3]) == Just (fromArray [1, 2, 3])
  assert $ NEA.insertAt 1 1 (fromArray [2, 3]) == Just (fromArray [2, 1, 3])
  assert $ NEA.insertAt 2 1 (fromArray [2, 3]) == Just (fromArray [2, 3, 1])

  log "insertAt should return Nothing if the index is out of A.range"
  assert $ (NEA.insertAt 2 1 (NEA.singleton 1)) == Nothing

  log "deleteAt should remove an item at the specified index"
  assert $ (NEA.deleteAt 0 (fromArray [1, 2, 3])) == Just [2, 3]
  assert $ (NEA.deleteAt 1 (fromArray [1, 2, 3])) == Just [1, 3]

  log "deleteAt should return Nothing if the index is out of A.range"
  assert $ (NEA.deleteAt 1 (NEA.singleton 1)) == Nothing

  log "updateAt should replace an item at the specified index"
  assert $ NEA.updateAt 0 9 (fromArray [1, 2, 3]) == Just (fromArray [9, 2, 3])
  assert $ NEA.updateAt 1 9 (fromArray [1, 2, 3]) == Just (fromArray [1, 9, 3])

  log "updateAt should return Nothing if the index is out of A.range"
  assert $ NEA.updateAt 1 9 (NEA.singleton 0) == Nothing

  log "modifyAt should update an item at the specified index"
  assert $ NEA.modifyAt 0 (_ + 1) (fromArray [1, 2, 3]) == Just (fromArray [2, 2, 3])
  assert $ NEA.modifyAt 1 (_ + 1) (fromArray [1, 2, 3]) == Just (fromArray [1, 3, 3])

  log "modifyAt should return Nothing if the index is out of A.range"
  assert $ NEA.modifyAt 1 (_ + 1) (NEA.singleton 0) == Nothing

  log "alterAt should update an item at the specified index when the function returns Just"
  assert $ NEA.alterAt 0 (Just <<< (_ + 1)) (fromArray [1, 2, 3]) == Just [2, 2, 3]
  assert $ NEA.alterAt 1 (Just <<< (_ + 1)) (fromArray [1, 2, 3]) == Just [1, 3, 3]

  log "alterAt should drop an item at the specified index when the function returns Nothing"
  assert $ NEA.alterAt 0 (const Nothing) (fromArray [1, 2, 3]) == Just [2, 3]
  assert $ NEA.alterAt 1 (const Nothing) (fromArray [1, 2, 3]) == Just [1, 3]

  log "alterAt should return Nothing if the index is out of NEA.range"
  assert $ NEA.alterAt 1 (Just <<< (_ + 1)) (NEA.singleton 1) == Nothing

  log "intersperse should return the original array when given an array with one element"
  assert $ NEA.intersperse " " (NEA.singleton "a") == NEA.singleton "a"

  log "intersperse should insert the given element in-between each element in an array with two or more elements"
  assert $ NEA.intersperse " " (fromArray ["a", "b"]) == fromArray ["a", " ", "b"]
  assert $ NEA.intersperse 0 (fromArray [ 1, 2, 3, 4, 5 ]) == fromArray [ 1, 0, 2, 0, 3, 0, 4, 0, 5 ]

  log "reverse should reverse the order of items in an array"
  assert $ NEA.reverse (fromArray [1, 2, 3]) == fromArray [3, 2, 1]
  assert $ NEA.reverse (NEA.singleton 0) == NEA.singleton 0

  log "concat should join an array of arrays"
  assert $ NEA.concat (fromArray [fromArray [1, 2], fromArray [3, 4]]) == fromArray [1, 2, 3, 4]

  log "concatMap should be equivalent to (concat <<< map)"
  assert $ NEA.concatMap doubleAndOrig (fromArray [1, 2, 3]) == NEA.concat (map doubleAndOrig (fromArray [1, 2, 3]))

  log "filter should remove items that don't match a predicate"
  assert $ NEA.filter odd (NEA.range 0 10) == [1, 3, 5, 7, 9]

  log "splitAt should split the array at the given number of elements"
  assert $ NEA.splitAt 3 (fromArray [1, 2, 3, 4, 5]) == { before: [1, 2, 3], after: [4, 5] }
  assert $ NEA.splitAt 1 (fromArray [1, 2, 3]) == { before: [1], after: [2, 3] }
  assert $ NEA.splitAt 3 (fromArray [1, 2, 3]) == { before: [1, 2, 3], after: [] }
  assert $ NEA.splitAt 4 (fromArray [1, 2, 3]) == { before: [1, 2, 3], after: [] }
  assert $ NEA.splitAt 0 (fromArray [1, 2, 3]) == { before: [], after: [1, 2, 3] }
  assert $ NEA.splitAt (-1) (fromArray [1, 2, 3]) == { before: [], after: [1, 2, 3] }

  log "filterA should remove items that don't match a predicate while using an applicative behaviour"
  assert $ NEA.filterA (Just <<< odd) (NEA.range 0 10) == Just [1, 3, 5, 7, 9]
  assert $ NEA.filterA (const Nothing) (NEA.range 0 10) == Nothing

  log "filterA should apply effects in the right order"
  assert $ NEA.filterA (Const <<< show) (NEA.range 1 5) == Const "12345"

  log "mapMaybe should transform every item in an array, throwing out Nothing values"
  assert $ NEA.mapMaybe (\x -> if x /= 0 then Just x else Nothing) (fromArray [0, 1, 0, 0, 2, 3]) == [1, 2, 3]

  log "catMaybe should take an array of Maybe values and throw out Nothings"
  assert $ NEA.catMaybes (fromArray [Nothing, Just 2, Nothing, Just 4]) == [2, 4]

  log "mapWithIndex applies a function with an index for every element"
  assert $ NEA.mapWithIndex (\i x -> x - i) (fromArray [9,8,7,6,5]) == fromArray [9,7,5,3,1]

  log "scanl should return an array that stores the accumulated value at each step"
  assert $ NEA.scanl (+)  0 (fromArray [1,2,3]) == fromArray [1, 3, 6]
  assert $ NEA.scanl (-) 10 (fromArray [1,2,3]) == fromArray [9, 7, 4]

  log "scanl should return the same results as its Foldable counterpart"
  assert $ NEA.scanl (+)  0 (fromArray [1,2,3]) == scanl (+)  0 (fromArray [1,2,3])
  assert $ NEA.scanl (-) 10 (fromArray [1,2,3]) == scanl (-) 10 (fromArray [1,2,3])

  log "scanr should return an array that stores the accumulated value at each step"
  assert $ NEA.scanr (+) 0 (fromArray [1,2,3]) == fromArray [6,5,3]
  assert $ NEA.scanr (flip (-)) 10 (fromArray [1,2,3]) == fromArray [4,5,7]

  log "scanr should return the same results as its Foldable counterpart"
  assert $ NEA.scanr (+) 0 (fromArray [1,2,3]) == scanr (+) 0 (fromArray [1,2,3])
  assert $ NEA.scanr (flip (-)) 10 (fromArray [1,2,3]) == scanr (flip (-)) 10 (fromArray [1,2,3])

  log "updateAtIndices changes the elements at specified indices"
  assert $ NEA.updateAtIndices
             [Tuple 0 false, Tuple 2 false, Tuple 8 false]
             (fromArray [true,  true, true,  true]) ==
             fromArray [false, true, false, true]

  log "modifyAtIndices modifies the elements at specified indices"
  assert $ NEA.modifyAtIndices [0, 2, 8] not (fromArray [true,  true, true,  true]) ==
                                           (fromArray [false, true, false, true])

  log "sort should reorder a list into ascending order based on the result of compare"
  assert $ NEA.sort (fromArray [1, 3, 2, 5, 6, 4]) == fromArray [1, 2, 3, 4, 5, 6]
  assert $ NEA.sort (fromArray [defined 1, undefined, defined 2]) == fromArray [undefined, defined 1, defined 2]

  log "sortBy should reorder a list into ascending order based on the result of a comparison function"
  assert $ NEA.sortBy (flip compare) (fromArray [1, 3, 2, 5, 6, 4]) == fromArray [6, 5, 4, 3, 2, 1]

  log "sortWith should reorder a list into ascending order based on the result of compare over a projection"
  assert $ NEA.sortWith identity (fromArray [1, 3, 2, 5, 6, 4]) == fromArray [1, 2, 3, 4, 5, 6]

  log "take should keep the specified number of items from the front of an array, discarding the rest"
  assert $ NEA.take 1 (fromArray [1, 2, 3]) == [1]
  assert $ NEA.take 2 (fromArray [1, 2, 3]) == [1, 2]

  log "takeWhile should keep all values that match a predicate from the front of an array"
  assert $ NEA.takeWhile (_ /= 2) (fromArray [1, 2, 3]) == [1]
  assert $ NEA.takeWhile (_ /= 3) (fromArray [1, 2, 3]) == [1, 2]

  log "take should keep the specified number of items from the end of an array, discarding the rest"
  assert $ NEA.takeEnd 1 (fromArray [1, 2, 3]) == [3]
  assert $ NEA.takeEnd 2 (fromArray [1, 2, 3]) == [2, 3]

  log "drop should remove the specified number of items from the front of an array"
  assert $ NEA.drop 1 (fromArray [1, 2, 3]) == [2, 3]
  assert $ NEA.drop 2 (fromArray [1, 2, 3]) == [3]

  log "dropWhile should remove all values that match a predicate from the front of an array"
  assert $ NEA.dropWhile (_ /= 1) (fromArray [1, 2, 3]) == [1, 2, 3]
  assert $ NEA.dropWhile (_ /= 2) (fromArray [1, 2, 3]) == [2, 3]

  log "drop should remove the specified number of items from the end of an array"
  assert $ NEA.dropEnd 1 (fromArray [1, 2, 3]) == [1, 2]
  assert $ NEA.dropEnd 2 (fromArray [1, 2, 3]) == [1]

  log "span should split an array in two based on a predicate"
  let testSpan { p, input, init_, rest_ } = do
        let result = NEA.span p input
        assert $ result.init == init_
        assert $ result.rest == rest_

  let oneToSeven = fromArray [1, 2, 3, 4, 5, 6, 7]
  testSpan { p: (_ < 4), input: oneToSeven, init_: [1, 2, 3], rest_: [4, 5, 6, 7] }

  log "group should group consecutive equal elements into arrays"
  assert $ NEA.group (fromArray [1, 2, 2, 3, 3, 3, 1]) == fromArray [NEA.singleton 1, fromArray [2, 2], fromArray [3, 3, 3], NEA.singleton 1]

  log "groupAll should group equal elements into arrays"
  assert $ NEA.groupAll (fromArray [1, 2, 2, 3, 3, 3, 1]) == fromArray [fromArray [1, 1], fromArray [2, 2], fromArray [3, 3, 3]]

  log "groupBy should group consecutive equal elements into arrays based on an equivalence relation"
  assert $ NEA.groupBy (\x y -> odd x && odd y) (fromArray [1, 1, 2, 2, 3, 3]) == fromArray [fromArray [1, 1], NEA.singleton 2, NEA.singleton 2, fromArray [3, 3]]

  log "groupBy should be stable"
  assert $ NEA.groupBy (\_ _ -> true) (fromArray [1, 2, 3]) == fromArray [fromArray [1, 2, 3]]

  log "groupAllBy should group equal elements into arrays based on the result of a comparison function"
  assert $ NEA.groupAllBy (comparing Down) (fromArray [1, 3, 2, 4, 3, 3]) == fromArray [nea [4], nea [3, 3, 3], nea [2], nea [1]]

  log "groupAllBy should be stable"
  assert $ NEA.groupAllBy (\_ _ -> EQ) (fromArray [1, 2, 3]) == fromArray [nea [1, 2, 3]]

  log "nub should remove duplicate elements from the list, keeping the first occurence"
  assert $ NEA.nub (fromArray [1, 2, 2, 3, 4, 1]) == fromArray [1, 2, 3, 4]

  log "nubEq should remove duplicate elements from the list, keeping the first occurence"
  assert $ NEA.nubEq (fromArray [1, 2, 2, 3, 4, 1]) == fromArray [1, 2, 3, 4]

  log "nubByEq should remove duplicate items from the list using a supplied predicate"
  let nubPred = \x y -> if odd x then false else x == y
  assert $ NEA.nubByEq nubPred (fromArray [1, 2, 2, 3, 3, 4, 4, 1]) == fromArray [1, 2, 3, 3, 4, 1]

  log "union should produce the union of two arrays"
  assert $ NEA.union (fromArray [1, 2, 3]) (fromArray [2, 3, 4]) == fromArray [1, 2, 3, 4]
  assert $ NEA.union (fromArray [1, 1, 2, 3]) (fromArray [2, 3, 4]) == fromArray [1, 1, 2, 3, 4]

  log "unionBy should produce the union of two arrays using the specified equality relation"
  assert $ NEA.unionBy (\_ y -> y < 5) (fromArray [1, 2, 3]) (fromArray [2, 3, 4, 5, 6]) == fromArray [1, 2, 3, 5, 6]

  log "delete should remove the first matching item from an array"
  assert $ NEA.delete 1 (fromArray [1, 2, 1]) == [2, 1]
  assert $ NEA.delete 2 (fromArray [1, 2, 1]) == [1, 1]

  log "deleteBy should remove the first equality-relation-matching item from an array"
  assert $ NEA.deleteBy (/=) 2 (fromArray [1, 2, 1]) == [2, 1]
  assert $ NEA.deleteBy (/=) 1 (fromArray [1, 2, 1]) == [1, 1]

  log "intersect should return the intersection of two arrays"
  assert $ NEA.intersect (fromArray [1, 2, 3, 4, 3, 2, 1]) (fromArray [1, 1, 2, 3]) == [1, 2, 3, 3, 2, 1]

  log "intersectBy should return the intersection of two arrays using the specified equivalence relation"
  assert $ NEA.intersectBy (\x y -> (x * 2) == y) (fromArray [1, 2, 3]) (fromArray [2, 6]) == [1, 3]

  log "zipWith should use the specified function to zip two arrays together"
  assert $ NEA.zipWith (\x y -> [show x, y]) (fromArray [1, 2, 3]) (fromArray ["a", "b", "c"]) == fromArray [["1", "a"], ["2", "b"], ["3", "c"]]

  log "zipWithA should use the specified function to zip two lists together"
  assert $ NEA.zipWithA (\x y -> Just $ Tuple x y) (fromArray [1, 2, 3]) (fromArray ["a", "b", "c"]) == Just (fromArray [Tuple 1 "a", Tuple 2 "b", Tuple 3 "c"])

  log "zip should use the specified function to zip two lists together"
  assert $ NEA.zip (fromArray [1, 2, 3]) (fromArray ["a", "b", "c"]) == fromArray [Tuple 1 "a", Tuple 2 "b", Tuple 3 "c"]

  log "unzip should deconstruct a list of tuples into a tuple of lists"
  assert $ NEA.unzip (fromArray [Tuple 1 "a", Tuple 2 "b", Tuple 3 "c"]) == Tuple (fromArray [1, 2, 3]) (fromArray ["a", "b", "c"])

  log "any should return true if at least one array element satisfy the given predicate"
  assert $ NEA.any (_ > 0) $ fromArray [-1, 0, 1]
  assert $ not $ NEA.any (_ > 0) $ fromArray [-1, -2, -3]

  log "all should return true if all the array elements satisfy the given predicate"
  assert $ NEA.all (_ > 0) $ fromArray [1, 2, 3]
  assert $ not $ NEA.all (_ > 0) $ fromArray [-1, -2, -3]

  log "fromFoldable"
  for_ (fromArray [[], [1], [1,2], [1,2,3,4,5]]) \xs -> do
    assert $ NEA.fromFoldable xs == NEA.fromArray xs

  log "toUnfoldable"
  let toUnfoldableId xs = NEA.toUnfoldable xs == NEA.toArray xs
  traverse_ (assert <<< toUnfoldableId) $
    fromArray (
      [ fromArray [1]
      , fromArray [1,2,3]
      , fromArray [2,3,1]
      , fromArray [4,0,0,1,25,36,458,5842,23757]
      ])

  log "toUnfoldable1"
  assert $ NEA.toUnfoldable1 (NEA.range 0 9) == 0 :| A.range 1 9

  log "Unfoldable instance"
  assert $ U1.range 0 9 == NEA.range 0 9

  log "foldMap1 should work"
  assert $ foldMap1 Additive (fromArray [1, 2, 3, 4]) == Additive 10

  log "fold1 should work"
  -- test through sum
  assert $ sum (fromArray [1, 2, 3, 4]) == 10

  log "foldr1 should work"
  assert $ foldr1 (\l r -> "(" <> l <> r <> ")") (fromArray ["a", "b", "c", "d"]) == "(a(b(cd)))"
  assert $ foldr1 (\l r -> "(" <> l <> r <> ")") (fromArray ["a", "b"])           == "(ab)"
  assert $ foldr1 (\l r -> "(" <> l <> r <> ")") (fromArray ["a"])                == "a"

  log "foldl1 should work"
  assert $ foldl1 (\l r -> "(" <> l <> r <> ")") (fromArray ["a", "b", "c", "d"]) == "(((ab)c)d)"
  assert $ foldl1 (\l r -> "(" <> l <> r <> ")") (fromArray ["a", "b"])           == "(ab)"
  assert $ foldl1 (\l r -> "(" <> l <> r <> ")") (fromArray ["a"])                == "a"

  log "traverse1 should work"
  assert $ traverse1 Just (fromArray [1, 2, 3, 4]) == NEA.fromArray [1, 2, 3, 4]

odd :: Int -> Boolean
odd n = n `mod` 2 /= zero

doubleAndOrig :: Int -> NEA.NonEmptyArray Int
doubleAndOrig x = NEA.cons (x * 2) (NEA.singleton x)
