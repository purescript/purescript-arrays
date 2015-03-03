# Module Documentation

## Module Data.Array

#### `(!!)`

``` purescript
(!!) :: forall a. [a] -> Number -> Maybe a
```


#### `snoc`

``` purescript
snoc :: forall a. [a] -> a -> [a]
```


#### `singleton`

``` purescript
singleton :: forall a. a -> [a]
```


#### `head`

``` purescript
head :: forall a. [a] -> Maybe a
```


#### `last`

``` purescript
last :: forall a. [a] -> Maybe a
```


#### `tail`

``` purescript
tail :: forall a. [a] -> Maybe [a]
```


#### `init`

``` purescript
init :: forall a. [a] -> Maybe [a]
```


#### `null`

``` purescript
null :: forall a. [a] -> Boolean
```


#### `length`

``` purescript
length :: forall a. [a] -> Number
```


#### `findIndex`

``` purescript
findIndex :: forall a. (a -> Boolean) -> [a] -> Number
```


#### `findLastIndex`

``` purescript
findLastIndex :: forall a. (a -> Boolean) -> [a] -> Number
```


#### `elemIndex`

``` purescript
elemIndex :: forall a. (Eq a) => a -> [a] -> Number
```


#### `elemLastIndex`

``` purescript
elemLastIndex :: forall a. (Eq a) => a -> [a] -> Number
```


#### `append`

``` purescript
append :: forall a. [a] -> [a] -> [a]
```


#### `concat`

``` purescript
concat :: forall a. [[a]] -> [a]
```


#### `reverse`

``` purescript
reverse :: forall a. [a] -> [a]
```


#### `drop`

``` purescript
drop :: forall a. Number -> [a] -> [a]
```


#### `take`

``` purescript
take :: forall a. Number -> [a] -> [a]
```


#### `insertAt`

``` purescript
insertAt :: forall a. Number -> a -> [a] -> [a]
```


#### `deleteAt`

``` purescript
deleteAt :: forall a. Number -> Number -> [a] -> [a]
```


#### `updateAt`

``` purescript
updateAt :: forall a. Number -> a -> [a] -> [a]
```


#### `deleteBy`

``` purescript
deleteBy :: forall a. (a -> a -> Boolean) -> a -> [a] -> [a]
```


#### `delete`

``` purescript
delete :: forall a. (Eq a) => a -> [a] -> [a]
```


#### `(\\)`

``` purescript
(\\) :: forall a. (Eq a) => [a] -> [a] -> [a]
```


#### `intersectBy`

``` purescript
intersectBy :: forall a. (a -> a -> Boolean) -> [a] -> [a] -> [a]
```


#### `intersect`

``` purescript
intersect :: forall a. (Eq a) => [a] -> [a] -> [a]
```


#### `concatMap`

``` purescript
concatMap :: forall a b. (a -> [b]) -> [a] -> [b]
```


#### `map`

``` purescript
map :: forall a b. (a -> b) -> [a] -> [b]
```


#### `mapMaybe`

``` purescript
mapMaybe :: forall a b. (a -> Maybe b) -> [a] -> [b]
```


#### `catMaybes`

``` purescript
catMaybes :: forall a. [Maybe a] -> [a]
```


#### `filter`

``` purescript
filter :: forall a. (a -> Boolean) -> [a] -> [a]
```


#### `range`

``` purescript
range :: Number -> Number -> [Number]
```


#### `(..)`

``` purescript
(..) :: Number -> Number -> [Number]
```


#### `zipWith`

``` purescript
zipWith :: forall a b c. (a -> b -> c) -> [a] -> [b] -> [c]
```


#### `nub`

``` purescript
nub :: forall a. (Eq a) => [a] -> [a]
```


#### `nubBy`

``` purescript
nubBy :: forall a. (a -> a -> Boolean) -> [a] -> [a]
```


#### `sort`

``` purescript
sort :: forall a. (Ord a) => [a] -> [a]
```


#### `sortBy`

``` purescript
sortBy :: forall a. (a -> a -> Ordering) -> [a] -> [a]
```


#### `group`

``` purescript
group :: forall a. (Eq a) => [a] -> [[a]]
```


#### `group'`

``` purescript
group' :: forall a. (Ord a) => [a] -> [[a]]
```

Performs a sorting first.

#### `groupBy`

``` purescript
groupBy :: forall a. (a -> a -> Boolean) -> [a] -> [[a]]
```


#### `span`

``` purescript
span :: forall a. (a -> Boolean) -> [a] -> { rest :: [a], init :: [a] }
```


#### `takeWhile`

``` purescript
takeWhile :: forall a. (a -> Boolean) -> [a] -> [a]
```


#### `dropWhile`

``` purescript
dropWhile :: forall a. (a -> Boolean) -> [a] -> [a]
```


#### `functorArray`

``` purescript
instance functorArray :: Functor Prim.Array
```


#### `applyArray`

``` purescript
instance applyArray :: Apply Prim.Array
```


#### `applicativeArray`

``` purescript
instance applicativeArray :: Applicative Prim.Array
```


#### `bindArray`

``` purescript
instance bindArray :: Bind Prim.Array
```


#### `monadArray`

``` purescript
instance monadArray :: Monad Prim.Array
```


#### `semigroupArray`

``` purescript
instance semigroupArray :: Semigroup [a]
```


#### `altArray`

``` purescript
instance altArray :: Alt Prim.Array
```


#### `plusArray`

``` purescript
instance plusArray :: Plus Prim.Array
```


#### `alternativeArray`

``` purescript
instance alternativeArray :: Alternative Prim.Array
```


#### `monadPlusArray`

``` purescript
instance monadPlusArray :: MonadPlus Prim.Array
```



## Module Data.Array.ST

#### `STArray`

``` purescript
data STArray :: * -> * -> *
```


#### `Assoc`

``` purescript
type Assoc a = { index :: Number, value :: a }
```


#### `runSTArray`

``` purescript
runSTArray :: forall a r. (forall h. Eff (st :: ST h | r) (STArray h a)) -> Eff r [a]
```


#### `emptySTArray`

``` purescript
emptySTArray :: forall a h r. Eff (st :: ST h | r) (STArray h a)
```


#### `peekSTArray`

``` purescript
peekSTArray :: forall a h r. STArray h a -> Number -> Eff (st :: ST h | r) (Maybe a)
```


#### `pokeSTArray`

``` purescript
pokeSTArray :: forall a h r. STArray h a -> Number -> a -> Eff (st :: ST h | r) Boolean
```


#### `pushAllSTArray`

``` purescript
pushAllSTArray :: forall a h r. STArray h a -> [a] -> Eff (st :: ST h | r) Number
```


#### `pushSTArray`

``` purescript
pushSTArray :: forall a h r. STArray h a -> a -> Eff (st :: ST h | r) Number
```


#### `spliceSTArray`

``` purescript
spliceSTArray :: forall a h r. STArray h a -> Number -> Number -> [a] -> Eff (st :: ST h | r) [a]
```


#### `freeze`

``` purescript
freeze :: forall a h r. STArray h a -> Eff (st :: ST h | r) [a]
```


#### `thaw`

``` purescript
thaw :: forall a h r. [a] -> Eff (st :: ST h | r) (STArray h a)
```


#### `toAssocArray`

``` purescript
toAssocArray :: forall a h r. STArray h a -> Eff (st :: ST h | r) [Assoc a]
```



## Module Data.Array.Unsafe

#### `head`

``` purescript
head :: forall a. [a] -> a
```


#### `tail`

``` purescript
tail :: forall a. [a] -> [a]
```


#### `last`

``` purescript
last :: forall a. [a] -> a
```


#### `init`

``` purescript
init :: forall a. [a] -> [a]
```