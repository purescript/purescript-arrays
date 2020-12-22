module Test.Data.UndefinedOr where

import Prelude

foreign import data UndefinedOr :: Type -> Type

foreign import undefined :: forall a. UndefinedOr a
foreign import defined :: forall a. a -> UndefinedOr a

foreign import eqUndefinedOrImpl :: forall a. (a -> a -> Boolean) -> UndefinedOr a -> UndefinedOr a -> Boolean
foreign import compareUndefinedOrImpl :: forall a. Ordering -> Ordering -> Ordering -> (a -> a -> Ordering) -> UndefinedOr a -> UndefinedOr a -> Ordering

instance eqUndefinedOr :: Eq a => Eq (UndefinedOr a) where
  eq = eqUndefinedOrImpl eq

instance ordUndefinedOr :: Ord a => Ord (UndefinedOr a) where
  compare = compareUndefinedOrImpl LT EQ GT compare
