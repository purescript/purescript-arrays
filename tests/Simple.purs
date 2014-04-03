module SimpleTests where

import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Data.Array
import Data.Maybe
import Data.Tuple
import Debug.Trace

assertEq :: forall a. (Show a, Eq a) => a -> a -> Eff (err :: Exception String, trace :: Trace) {}
assertEq x y =
  if x /= y
    then throwException $ "Assertion failed: " ++ show x ++ " /= " ++ show y
    else trace $ "Assertion passed"
    
double :: Number -> Number
double x = x * 2

doubleAndOrig :: Number -> [Number]
doubleAndOrig x = [x * 2, x]

nil :: [Number]
nil = []

odd :: Number -> Boolean
odd n = n % 2 /= 0

main = do

  -- TODO: use quickcheck where appropriate

  assertEq (head [1, 2, 3]) (Just 1)
  assertEq (head nil) Nothing
  
  assertEq (tail [1, 2, 3]) (Just [2, 3])
  assertEq (tail nil) Nothing
  
  assertEq (map double [1, 2, 3]) [2, 4, 6]
  
  assertEq (length [1, 2, 3]) 3
  assertEq (length nil) 0
  
  assertEq (indexOf [1, 2, 1] 1) 0
  assertEq (indexOf [1, 2, 1] 4) (-1)

  assertEq (lastIndexOf [1, 2, 1] 1) 2
  assertEq (lastIndexOf [1, 2, 1] 4) (-1)

  assertEq (concat [1, 2] [3, 4]) [1, 2, 3, 4]
  assertEq (concat [1] nil) [1]
  assertEq (concat nil nil) nil

  assertEq (joinWith ["foo", "bar"] " / ") "foo / bar"

  assertEq (reverse [1, 2, 3]) [3, 2, 1]
  assertEq (reverse nil) nil

  assertEq (drop 1 [1, 2, 3]) [2, 3]
  assertEq (drop 1 nil) nil

  assertEq (take 1 [1, 2, 3]) [1]
  assertEq (take 1 nil) nil

  assertEq (slice 1 3 [1, 2, 3, 4]) [2, 3]
  assertEq (slice 0 0 [1, 2, 3, 4]) nil
  assertEq (slice 0 1 nil) nil
  
  assertEq (insertAt 0 1 [2, 3]) [1, 2, 3]
  assertEq (insertAt 0 1 nil) [1]
  
  assertEq (deleteAt 0 1 [1, 2, 3]) [2, 3]
  assertEq (deleteAt 0 1 nil) nil
  
  assertEq (updateAt 1 999 [1, 2, 3]) [1, 999, 3]
  
  assertEq (concatMap doubleAndOrig [1, 2, 3]) [2, 1, 4, 2, 6, 3]
  
  assertEq (filter odd [1, 2, 3]) [1, 3]
  
  assertEq (isEmpty [1, 2, 3]) false
  assertEq (isEmpty nil) true
  
  assertEq (range 0 5) [0, 1, 2, 3, 4, 5]
  
  assertEq (zipWith Tuple [1, 2, 3] ["a", "b", "c"]) [Tuple 1 "a", Tuple 2 "b", Tuple 3 "c"]
  
  assertEq (nub [1, 2, 2, 3, 4, 1]) [1, 2, 3, 4]
  
  assertEq (sort [1, 3, 2, 5, 6, 4]) [1, 2, 3, 4, 5, 6]
