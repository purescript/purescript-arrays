module Data.Array.NonEmpty
  ( NonEmptyArray
  , fromArray
  , fromNonEmpty
  , toArray
  , toNonEmpty

  , fromFoldable
  , fromFoldable1
  , toUnfoldable
  , toUnfoldable1
  , singleton
  , (..), range
  , replicate
  , some

  , length

  , (:), cons
  , cons'
  , snoc
  , snoc'
  , appendArray
  , insert
  , insertBy

  , head
  , last
  , tail
  , init
  , uncons
  , unsnoc

  , (!!), index
  , elemIndex
  , elemLastIndex
  , findIndex
  , findLastIndex
  , insertAt
  , deleteAt
  , updateAt
  , updateAtIndices
  , modifyAt
  , modifyAtIndices
  , alterAt

  , reverse
  , concat
  , concatMap
  , filter
  , partition
  , filterA
  , mapMaybe
  , catMaybes

  , sort
  , sortBy
  , sortWith
  , slice
  , take
  , takeEnd
  , takeWhile
  , drop
  , dropEnd
  , dropWhile
  , span

  , nub
  , nubBy
  , union
  , union'
  , unionBy
  , unionBy'
  , delete
  , deleteBy

  , (\\), difference
  , difference'
  , intersect
  , intersect'
  , intersectBy
  , intersectBy'

  , zipWith
  , zipWithA
  , zip
  , unzip

  , foldM
  , foldRecM

  , unsafeIndex
  ) where

import Prelude

import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Control.Lazy (class Lazy)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Array as A
import Data.Bifunctor (bimap)
import Data.Eq (class Eq1)
import Data.Foldable (class Foldable)
import Data.FoldableWithIndex (class FoldableWithIndex)
import Data.FunctorWithIndex (class FunctorWithIndex)
import Data.Maybe (Maybe(..), fromJust, isNothing)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Ord (class Ord1)
import Data.Semigroup.Foldable (class Foldable1, foldMap1Default)
import Data.Semigroup.Traversable (class Traversable1, sequence1Default)
import Data.Traversable (class Traversable)
import Data.TraversableWithIndex (class TraversableWithIndex)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Unfoldable (class Unfoldable)
import Data.Unfoldable1 (class Unfoldable1, unfoldr1)
import Partial.Unsafe (unsafePartial)

newtype NonEmptyArray a = NonEmptyArray (Array a)

instance showNonEmptyArray :: Show a => Show (NonEmptyArray a) where
  show (NonEmptyArray xs) = "(NonEmptyArray " <> show xs <> ")"

derive newtype instance eqNonEmptyArray :: Eq a => Eq (NonEmptyArray a)
derive newtype instance eq1NonEmptyArray :: Eq1 NonEmptyArray

derive newtype instance ordNonEmptyArray :: Ord a => Ord (NonEmptyArray a)
derive newtype instance ord1NonEmptyArray :: Ord1 NonEmptyArray 

derive newtype instance semigroupNonEmptyArray :: Semigroup (NonEmptyArray a)

derive newtype instance functorNonEmptyArray :: Functor NonEmptyArray
derive newtype instance functorWithIndexNonEmptyArray :: FunctorWithIndex Int NonEmptyArray

derive newtype instance foldableNonEmptyArray :: Foldable NonEmptyArray
derive newtype instance foldableWithIndexNonEmptyArray :: FoldableWithIndex Int NonEmptyArray

instance foldable1NonEmptyArray :: Foldable1 NonEmptyArray where
  foldMap1 = foldMap1Default
  fold1 = fold1Impl (<>)

instance unfoldable1NonEmptyArray :: Unfoldable1 NonEmptyArray where
  unfoldr1 = unfoldr1Impl isNothing (unsafePartial fromJust) fst snd

derive newtype instance traversableNonEmptyArray :: Traversable NonEmptyArray
derive newtype instance traversableWithIndexNonEmptyArray :: TraversableWithIndex Int NonEmptyArray

instance traversable1NonEmptyArray :: Traversable1 NonEmptyArray where
  traverse1 = traverse1Impl apply map
  sequence1 = sequence1Default

derive newtype instance applyNonEmptyArray :: Apply NonEmptyArray

derive newtype instance applicativeNonEmptyArray :: Applicative NonEmptyArray

derive newtype instance bindNonEmptyArray :: Bind NonEmptyArray

derive newtype instance monadNonEmptyArray :: Monad NonEmptyArray

derive newtype instance altNonEmptyArray :: Alt NonEmptyArray

foreign import unfoldr1Impl
  :: forall a b
   . (forall x. Maybe x -> Boolean)
  -> (forall x. Maybe x -> x)
  -> (forall x y. Tuple x y -> x)
  -> (forall x y. Tuple x y -> y)
  -> (b -> Tuple a (Maybe b))
  -> b
  -> NonEmptyArray a

-- | Internal - adapt an Array transform to NonEmptyArray
--
-- Note that this is unsafe: if the transform returns an empty array, this can
-- explode at runtime.
unsafeAdapt :: forall a b. (Array a -> Array b) -> NonEmptyArray a -> NonEmptyArray b
unsafeAdapt f = NonEmptyArray <<< adaptAny f

-- | Internal - adapt an Array transform to NonEmptyArray,
--   with polymorphic result.
--
-- Note that this is unsafe: if the transform returns an empty array, this can
-- explode at runtime.
adaptAny :: forall a b. (Array a -> b) -> NonEmptyArray a -> b
adaptAny f = f <<< toArray

-- | Internal - adapt Array functions returning Maybes to NonEmptyArray
adaptMaybe :: forall a b. (Array a -> Maybe b) -> NonEmptyArray a -> b
adaptMaybe f = unsafePartial $ fromJust <<< f <<< toArray

fromArray :: forall a. Array a -> Maybe (NonEmptyArray a)
fromArray xs
  | A.length xs > 0 = Just (NonEmptyArray xs)
  | otherwise       = Nothing

-- | INTERNAL
unsafeFromArray :: forall a. Array a -> NonEmptyArray a
unsafeFromArray = NonEmptyArray

fromNonEmpty :: forall a. NonEmpty Array a -> NonEmptyArray a
fromNonEmpty (x :| xs) = cons' x xs

toArray :: forall a. NonEmptyArray a -> Array a
toArray (NonEmptyArray xs) = xs

toNonEmpty :: forall a. NonEmptyArray a -> NonEmpty Array a
toNonEmpty = uncons >>> \{head: x, tail: xs} -> x :| xs

fromFoldable :: forall f a. Foldable f => f a -> Maybe (NonEmptyArray a)
fromFoldable = fromArray <<< A.fromFoldable

fromFoldable1 :: forall f a. Foldable1 f => f a -> NonEmptyArray a
fromFoldable1 = unsafeFromArray <<< A.fromFoldable

toUnfoldable :: forall f a. Unfoldable f => NonEmptyArray a -> f a
toUnfoldable = adaptAny A.toUnfoldable

toUnfoldable1 :: forall f a. Unfoldable1 f => NonEmptyArray a -> f a
toUnfoldable1 xs = unfoldr1 f 0
  where
  len = length xs
  f i = Tuple (unsafePartial unsafeIndex xs i) $
          if i < (len - 1) then Just (i + 1) else Nothing

singleton :: forall a. a -> NonEmptyArray a
singleton = NonEmptyArray <<< A.singleton

range :: Int -> Int -> NonEmptyArray Int
range x y = unsafeFromArray $ A.range x y

infix 8 range as ..

-- | Replicate an item at least once
replicate :: forall a. Int -> a -> NonEmptyArray a
replicate i x = NonEmptyArray $ A.replicate (max 1 i) x

some
  :: forall f a
   . Alternative f
  => Lazy (f (Array a))
  => f a -> f (NonEmptyArray a)
some = map NonEmptyArray <<< A.some

length :: forall a. NonEmptyArray a -> Int
length = adaptAny A.length

cons :: forall a. a -> NonEmptyArray a -> NonEmptyArray a
cons x = unsafeAdapt $ A.cons x

infixr 6 cons as :

cons' :: forall a. a -> Array a -> NonEmptyArray a
cons' x xs = unsafeFromArray $ A.cons x xs

snoc :: forall a. NonEmptyArray a -> a -> NonEmptyArray a
snoc xs x = unsafeFromArray $ A.snoc (toArray xs) x

snoc' :: forall a. Array a -> a -> NonEmptyArray a
snoc' xs x = unsafeFromArray $ A.snoc xs x

appendArray :: forall a. NonEmptyArray a -> Array a -> NonEmptyArray a
appendArray xs ys = unsafeFromArray $ toArray xs <> ys

insert :: forall a. Ord a => a -> NonEmptyArray a -> NonEmptyArray a
insert x = unsafeAdapt $ A.insert x

insertBy :: forall a. (a -> a -> Ordering) -> a -> NonEmptyArray a -> NonEmptyArray a
insertBy f x = unsafeAdapt $ A.insertBy f x

head :: forall a. NonEmptyArray a -> a
head = adaptMaybe A.head

last :: forall a. NonEmptyArray a -> a
last = adaptMaybe A.last

tail :: forall a. NonEmptyArray a -> Array a
tail = adaptMaybe A.tail

init :: forall a. NonEmptyArray a -> Array a
init = adaptMaybe A.init

uncons :: forall a. NonEmptyArray a -> { head :: a, tail :: Array a }
uncons = adaptMaybe A.uncons

unsnoc :: forall a. NonEmptyArray a -> { init :: Array a, last :: a }
unsnoc = adaptMaybe A.unsnoc

index :: forall a. NonEmptyArray a -> Int -> Maybe a
index = adaptAny A.index

infixl 8 index as !!

elemIndex :: forall a. Eq a => a -> NonEmptyArray a -> Maybe Int
elemIndex x = adaptAny $ A.elemIndex x

elemLastIndex :: forall a. Eq a => a -> NonEmptyArray a -> Maybe Int
elemLastIndex x = adaptAny $ A.elemLastIndex x

findIndex :: forall a. (a -> Boolean) -> NonEmptyArray a -> Maybe Int
findIndex x = adaptAny $ A.findIndex x

findLastIndex :: forall a. (a -> Boolean) -> NonEmptyArray a -> Maybe Int
findLastIndex x = adaptAny $ A.findLastIndex x

insertAt :: forall a. Int -> a -> NonEmptyArray a -> Maybe (NonEmptyArray a)
insertAt i x = map NonEmptyArray <<< A.insertAt i x <<< toArray

deleteAt :: forall a. Int -> NonEmptyArray a -> Maybe (Array a)
deleteAt i = adaptAny $ A.deleteAt i

updateAt :: forall a. Int -> a -> NonEmptyArray a -> Maybe (NonEmptyArray a)
updateAt i x = map NonEmptyArray <<< A.updateAt i x <<< toArray

updateAtIndices :: forall t a. Foldable t => t (Tuple Int a) -> NonEmptyArray a -> NonEmptyArray a
updateAtIndices pairs = unsafeAdapt $ A.updateAtIndices pairs

modifyAt :: forall a. Int -> (a -> a) -> NonEmptyArray a -> Maybe (NonEmptyArray a)
modifyAt i f = map NonEmptyArray <<< A.modifyAt i f <<< toArray

modifyAtIndices :: forall t a. Foldable t => t Int -> (a -> a) -> NonEmptyArray a -> NonEmptyArray a
modifyAtIndices is f = unsafeAdapt $ A.modifyAtIndices is f

alterAt :: forall a. Int -> (a -> Maybe a) -> NonEmptyArray a -> Maybe (Array a)
alterAt i f = A.alterAt i f <<< toArray

reverse :: forall a. NonEmptyArray a -> NonEmptyArray a
reverse = unsafeAdapt A.reverse

concat :: forall a. NonEmptyArray (NonEmptyArray a) -> NonEmptyArray a
concat = NonEmptyArray <<< A.concat <<< toArray <<< map toArray

concatMap :: forall a b. (a -> NonEmptyArray b) -> NonEmptyArray a -> NonEmptyArray b
concatMap = flip bind

filter :: forall a. (a -> Boolean) -> NonEmptyArray a -> Array a
filter f = adaptAny $ A.filter f

partition
  :: forall a
   . (a -> Boolean)
  -> NonEmptyArray a
  -> { yes :: Array a, no :: Array a}
partition f = adaptAny $ A.partition f

filterA
  :: forall a f
   . Applicative f
  => (a -> f Boolean)
  -> NonEmptyArray a
  -> f (Array a)
filterA f = adaptAny $ A.filterA f

mapMaybe :: forall a b. (a -> Maybe b) -> NonEmptyArray a -> Array b
mapMaybe f = adaptAny $ A.mapMaybe f

catMaybes :: forall a. NonEmptyArray (Maybe a) -> Array a
catMaybes = adaptAny A.catMaybes

sort :: forall a. Ord a => NonEmptyArray a -> NonEmptyArray a
sort = unsafeAdapt A.sort

sortBy :: forall a. (a -> a -> Ordering) -> NonEmptyArray a -> NonEmptyArray a
sortBy f = unsafeAdapt $ A.sortBy f

sortWith :: forall a b. Ord b => (a -> b) -> NonEmptyArray a -> NonEmptyArray a
sortWith f = unsafeAdapt $ A.sortWith f

slice :: forall a. Int -> Int -> NonEmptyArray a -> Array a
slice start end = adaptAny $ A.slice start end

take :: forall a. Int -> NonEmptyArray a -> Array a
take i = adaptAny $ A.take i

takeEnd :: forall a. Int -> NonEmptyArray a -> Array a
takeEnd i = adaptAny $ A.takeEnd i

takeWhile :: forall a. (a -> Boolean) -> NonEmptyArray a -> Array a
takeWhile f = adaptAny $ A.takeWhile f

drop :: forall a. Int -> NonEmptyArray a -> Array a
drop i = adaptAny $ A.drop i

dropEnd :: forall a. Int -> NonEmptyArray a -> Array a
dropEnd i = adaptAny $ A.dropEnd i

dropWhile :: forall a. (a -> Boolean) -> NonEmptyArray a -> Array a
dropWhile f = adaptAny $ A.dropWhile f

span
  :: forall a
   . (a -> Boolean)
  -> NonEmptyArray a
  -> { init :: Array a, rest :: Array a }
span f = adaptAny $ A.span f

nub :: forall a. Eq a => NonEmptyArray a -> NonEmptyArray a
nub = unsafeAdapt A.nub

nubBy :: forall a. (a -> a -> Boolean) -> NonEmptyArray a -> NonEmptyArray a
nubBy f = unsafeAdapt $ A.nubBy f

union :: forall a. Eq a => NonEmptyArray a -> NonEmptyArray a -> NonEmptyArray a
union = unionBy (==)

union' :: forall a. Eq a => NonEmptyArray a -> Array a -> NonEmptyArray a
union' = unionBy' (==)

unionBy
  :: forall a
   . (a -> a -> Boolean)
  -> NonEmptyArray a
  -> NonEmptyArray a
  -> NonEmptyArray a
unionBy eq xs = unionBy' eq xs <<< toArray

unionBy'
  :: forall a
   . (a -> a -> Boolean)
  -> NonEmptyArray a
  -> Array a
  -> NonEmptyArray a
unionBy' eq xs = unsafeFromArray <<< A.unionBy eq (toArray xs)

delete :: forall a. Eq a => a -> NonEmptyArray a -> Array a
delete x = adaptAny $ A.delete x

deleteBy :: forall a. (a -> a -> Boolean) -> a -> NonEmptyArray a -> Array a
deleteBy f x = adaptAny $ A.deleteBy f x

difference :: forall a. Eq a => NonEmptyArray a -> NonEmptyArray a -> Array a
difference xs = adaptAny $ difference' xs

difference' :: forall a. Eq a => NonEmptyArray a -> Array a -> Array a
difference' xs = A.difference $ toArray xs

intersect :: forall a . Eq a => NonEmptyArray a -> NonEmptyArray a -> Array a
intersect = intersectBy eq

intersect' :: forall a . Eq a => NonEmptyArray a -> Array a -> Array a
intersect' = intersectBy' eq

intersectBy
  :: forall a
   . (a -> a -> Boolean)
  -> NonEmptyArray a
  -> NonEmptyArray a
  -> Array a
intersectBy eq xs = intersectBy' eq xs <<< toArray

intersectBy'
  :: forall a
   . (a -> a -> Boolean)
  -> NonEmptyArray a
  -> Array a
  -> Array a
intersectBy' eq xs = A.intersectBy eq (toArray xs)

infix 5 difference as \\

zipWith
  :: forall a b c
   . (a -> b -> c)
  -> NonEmptyArray a
  -> NonEmptyArray b
  -> NonEmptyArray c
zipWith f xs ys = NonEmptyArray $ A.zipWith f (toArray xs) (toArray ys)


zipWithA
  :: forall m a b c
   . Applicative m
  => (a -> b -> m c)
  -> NonEmptyArray a
  -> NonEmptyArray b
  -> m (NonEmptyArray c)
zipWithA f xs ys = NonEmptyArray <$> A.zipWithA f (toArray xs) (toArray ys)

zip :: forall a b. NonEmptyArray a -> NonEmptyArray b -> NonEmptyArray (Tuple a b)
zip xs ys = NonEmptyArray $ toArray xs `A.zip` toArray ys

unzip :: forall a b. NonEmptyArray (Tuple a b) -> Tuple (NonEmptyArray a) (NonEmptyArray b)
unzip = bimap NonEmptyArray NonEmptyArray <<< A.unzip <<< toArray

foldM :: forall m a b. Monad m => (a -> b -> m a) -> a -> NonEmptyArray b -> m a
foldM f acc = adaptAny $ A.foldM f acc

foldRecM :: forall m a b. MonadRec m => (a -> b -> m a) -> a -> NonEmptyArray b -> m a
foldRecM f acc = adaptAny $ A.foldRecM f acc

unsafeIndex :: forall a. Partial => NonEmptyArray a -> Int -> a
unsafeIndex = adaptAny A.unsafeIndex

-- we use FFI here to avoid the unnecessary copy created by `tail`
foreign import fold1Impl :: forall a. (a -> a -> a) -> NonEmptyArray a -> a

foreign import traverse1Impl
  :: forall m a b
   . (forall a' b'. (m (a' -> b') -> m a' -> m b'))
  -> (forall a' b'. (a' -> b') -> m a' -> m b')
  -> (a -> m b)
  -> NonEmptyArray a
  -> m (NonEmptyArray b)
