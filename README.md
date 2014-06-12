# Module Documentation

## Module Data.Array

### Type Class Instances

    instance alternativeArray :: Alternative Prim.Array

    instance applicativeArray :: Applicative Prim.Array

    instance applyArray :: Apply Prim.Array

    instance bindArray :: Bind Prim.Array

    instance functorArray :: Functor Prim.Array

    instance monadArray :: Monad Prim.Array

    instance semigroupArray :: Semigroup [a]


### Values

    (!!) :: forall a. [a] -> Number -> Maybe a

    (\\) :: forall a. (Eq a) => [a] -> [a] -> [a]

    append :: forall a. [a] -> [a] -> [a]

    catMaybes :: forall a. [Maybe a] -> [a]

    concat :: forall a. [[a]] -> [a]

    concatMap :: forall a b. (a -> [b]) -> [a] -> [b]

    delete :: forall a. (Eq a) => a -> [a] -> [a]

    deleteAt :: forall a. Number -> Number -> [a] -> [a]

    deleteBy :: forall a. (a -> a -> Boolean) -> a -> [a] -> [a]

    drop :: forall a. Number -> [a] -> [a]

    elemIndex :: forall a. (Eq a) => a -> [a] -> Number

    elemLastIndex :: forall a. (Eq a) => a -> [a] -> Number

    filter :: forall a. (a -> Boolean) -> [a] -> [a]

    findIndex :: forall a. (a -> Boolean) -> [a] -> Number

    findLastIndex :: forall a. (a -> Boolean) -> [a] -> Number

    group :: forall a. (Eq a) => [a] -> [[a]]

    group' :: forall a. (Ord a) => [a] -> [[a]]

    groupBy :: forall a. (a -> a -> Boolean) -> [a] -> [[a]]

    head :: forall a. [a] -> Maybe a

    init :: forall a. [a] -> Maybe [a]

    insertAt :: forall a. Number -> a -> [a] -> [a]

    intersect :: forall a. (Eq a) => [a] -> [a] -> [a]

    intersectBy :: forall a. (a -> a -> Boolean) -> [a] -> [a] -> [a]

    last :: forall a. [a] -> Maybe a

    length :: forall a. [a] -> Number

    map :: forall a b. (a -> b) -> [a] -> [b]

    mapMaybe :: forall a b. (a -> Maybe b) -> [a] -> [b]

    nub :: forall a. (Eq a) => [a] -> [a]

    nubBy :: forall a. (a -> a -> Boolean) -> [a] -> [a]

    null :: forall a. [a] -> Boolean

    range :: Number -> Number -> [Number]

    reverse :: forall a. [a] -> [a]

    singleton :: forall a. a -> [a]

    snoc :: forall a. [a] -> a -> [a]

    sort :: forall a. (Ord a) => [a] -> [a]

    sortBy :: forall a. (a -> a -> Ordering) -> [a] -> [a]

    span :: forall a. (a -> Boolean) -> [a] -> { rest :: [a], init :: [a] }

    tail :: forall a. [a] -> Maybe [a]

    take :: forall a. Number -> [a] -> [a]

    updateAt :: forall a. Number -> a -> [a] -> [a]

    zipWith :: forall a b c. (a -> b -> c) -> [a] -> [b] -> [c]


## Module Data.Array.Unsafe

### Values

    head :: forall a. [a] -> a

    init :: forall a. [a] -> [a]

    last :: forall a. [a] -> a

    tail :: forall a. [a] -> [a]