module Data.Array.ST.Iterator
  ( Iterator
  , iterator
  , iterate
  , next
  , peek
  , exhausted
  , pushWhile
  , pushAll
  ) where

import Prelude
import Control.Monad.Eff (Eff, whileE)
import Control.Monad.ST (ST, STRef, newSTRef, readSTRef, writeSTRef, modifySTRef)
import Data.Array.ST (STArray, pushSTArray)

import Data.Maybe (Maybe(..), isNothing)

-- | This type provides a slightly easier way of iterating over an array's
-- | elements in an STArray computation, without having to keep track of
-- | indices.
data Iterator h a = Iterator (Int -> Maybe a) (STRef h Int)

-- | Make an Iterator given an indexing function into an array (or anything
-- | else). If `xs :: Array a`, the standard way to create an iterator over
-- | `xs` is to use `iterator (xs !! _)`, where `(!!)` comes from `Data.Array`.
iterator :: forall a h r. (Int -> Maybe a) -> Eff (st :: ST h | r) (Iterator h a)
iterator f =
  Iterator f <$> newSTRef 0

-- | Perform an action once for each item left in an iterator. If the action
-- | itself also advances the same iterator, `iterate` will miss those items
-- | out.
iterate :: forall a h r. Iterator h a -> (a -> Eff (st :: ST h | r) Unit) -> Eff (st :: ST h | r) Unit
iterate iter f = do
  break <- newSTRef false
  whileE (not <$> readSTRef break) do
    mx <- next iter
    case mx of
      Just x -> f x
      Nothing -> void $ writeSTRef break true

-- | Get the next item out of an iterator, advancing it. Returns Nothing if the
-- | Iterator is exhausted.
next :: forall a h r. Iterator h a -> Eff (st :: ST h | r) (Maybe a)
next (Iterator f currentIndex) = do
  i <- readSTRef currentIndex
  modifySTRef currentIndex (_ + 1)
  pure (f i)

-- | Get the next item out of an iterator without advancing it.
peek :: forall a h r. Iterator h a -> Eff (st :: ST h | r) (Maybe a)
peek (Iterator f currentIndex) = do
  i <- readSTRef currentIndex
  pure (f i)

-- | Check whether an iterator has been exhausted.
exhausted :: forall a h r. Iterator h a -> Eff (st :: ST h | r) Boolean
exhausted = map isNothing <<< peek

-- | Extract elements from an iterator and push them on to an STArray for as
-- | long as those elements satisfy a given predicate.
pushWhile :: forall a h r. (a -> Boolean) -> Iterator h a -> STArray h a -> Eff (st :: ST h | r) Unit
pushWhile p iter array = do
  break <- newSTRef false
  whileE (not <$> readSTRef break) do
    mx <- peek iter
    case mx of
      Just x | p x -> do
        pushSTArray array x
        void $ next iter
      _ ->
        void $ writeSTRef break true

-- | Push the entire remaining contents of an iterator onto an STArray.
pushAll :: forall a h r. Iterator h a -> STArray h a -> Eff (st :: ST h | r) Unit
pushAll = pushWhile (const true)
