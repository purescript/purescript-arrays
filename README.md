# Module Documentation

## Module Data.Array

### Type Class Instances

    instance altArray :: Alt Prim.Array

    instance alternativeArray :: Alternative Prim.Array

    instance applicativeArray :: Applicative Prim.Array

    instance applyArray :: Apply Prim.Array

    instance bindArray :: Bind Prim.Array

    instance functorArray :: Functor Prim.Array

    instance monadArray :: Monad Prim.Array

    instance monadPlusArray :: MonadPlus Prim.Array

    instance plusArray :: Plus Prim.Array

    instance semigroupArray :: Semigroup [a]


### Values

    (!!) :: forall a. [a] -> Number -> Maybe a

    (..) :: Number -> Number -> [Number]

    (\\) :: forall a. (Eq a) => [a] -> [a] -> [a]

    all :: forall a. (a -> Boolean) -> [a] -> Boolean

    any :: forall a. (a -> Boolean) -> [a] -> Boolean

    append :: forall a. [a] -> [a] -> [a]

    break :: forall a. (a -> Boolean) -> [a] -> { rest :: [a], init :: [a] }

    catMaybes :: forall a. [Maybe a] -> [a]

    concat :: forall a. [[a]] -> [a]

    concatMap :: forall a b. (a -> [b]) -> [a] -> [b]

    delete :: forall a. (Eq a) => a -> [a] -> [a]

    deleteAt :: forall a. Number -> Number -> [a] -> [a]

    deleteBy :: forall a. (a -> a -> Boolean) -> a -> [a] -> [a]

    drop :: forall a. Number -> [a] -> [a]

    elemIndex :: forall a. (Eq a) => a -> [a] -> Number

    elemLastIndex :: forall a. (Eq a) => a -> [a] -> Number

    fill :: forall a. Number -> a -> [a]

    filter :: forall a. (a -> Boolean) -> [a] -> [a]

    findIndex :: forall a. (a -> Boolean) -> [a] -> Number

    findLastIndex :: forall a. (a -> Boolean) -> [a] -> Number

    group :: forall a. (Eq a) => [a] -> [[a]]

    group' :: forall a. (Ord a) => [a] -> [[a]]

    groupBy :: forall a. (a -> a -> Boolean) -> [a] -> [[a]]

    head :: forall a. [a] -> Maybe a

    init :: forall a. [a] -> Maybe [a]

    insertAt :: forall a. Number -> a -> [a] -> [a]

    intercalate :: forall a. [a] -> [[a]] -> [a]

    intersect :: forall a. (Eq a) => [a] -> [a] -> [a]

    intersectBy :: forall a. (a -> a -> Boolean) -> [a] -> [a] -> [a]

    intersperse :: forall a. a -> [a] -> [a]

    last :: forall a. [a] -> Maybe a

    length :: forall a. [a] -> Number

    map :: forall a b. (a -> b) -> [a] -> [b]

    mapMaybe :: forall a b. (a -> Maybe b) -> [a] -> [b]

    nub :: forall a. (Eq a) => [a] -> [a]

    nubBy :: forall a. (a -> a -> Boolean) -> [a] -> [a]

    null :: forall a. [a] -> Boolean

    range :: Number -> Number -> [Number]

    replicate :: forall a. Number -> a -> [a]

    reverse :: forall a. [a] -> [a]

    singleton :: forall a. a -> [a]

    snoc :: forall a. [a] -> a -> [a]

    sort :: forall a. (Ord a) => [a] -> [a]

    sortBy :: forall a. (a -> a -> Ordering) -> [a] -> [a]

    span :: forall a. (a -> Boolean) -> [a] -> { rest :: [a], init :: [a] }

    tabulate :: forall a. Number -> (Number -> a) -> [a]

    tail :: forall a. [a] -> Maybe [a]

    take :: forall a. Number -> [a] -> [a]

    updateAt :: forall a. Number -> a -> [a] -> [a]

    zipWith :: forall a b c. (a -> b -> c) -> [a] -> [b] -> [c]


## Module Data.Array.ST

### Types

    data STArray :: * -> * -> *


### Values

    emptySTArray :: forall a h r. Eff (st :: ST h | r) (STArray h a)

    peekSTArray :: forall a h r. STArray h a -> Number -> Eff (st :: ST h | r) (Maybe a)

    pokeSTArray :: forall a h r. STArray h a -> Number -> a -> Eff (st :: ST h | r) Boolean

    pushSTArray :: forall a h r. STArray h a -> a -> Eff (st :: ST h | r) Unit

    runSTArray :: forall a r. (forall h. Eff (st :: ST h | r) (STArray h a)) -> Eff r [a]


## Module Data.Array.Unsafe

### Values

    head :: forall a. [a] -> a

    init :: forall a. [a] -> [a]

    last :: forall a. [a] -> a

    tail :: forall a. [a] -> [a]