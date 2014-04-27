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

    (!!) :: forall a. [a] -> Prim.Number -> Maybe a

    append :: forall a. [a] -> [a] -> [a]

    concat :: forall a. [[a]] -> [a]

    concatMap :: forall a b. (a -> [b]) -> [a] -> [b]

    deleteAt :: forall a. Prim.Number -> Prim.Number -> [a] -> [a]

    drop :: forall a. Prim.Number -> [a] -> [a]

    elem :: forall a. a -> [a] -> Prim.Boolean

    elemIndex :: forall a. a -> [a] -> Prim.Number

    elemLastIndex :: forall a. a -> [a] -> Prim.Number

    filter :: forall a. (a -> Prim.Boolean) -> [a] -> [a]

    head :: forall a. [a] -> Maybe a

    init :: forall a. [a] -> Maybe [a]

    insertAt :: forall a. Prim.Number -> a -> [a] -> [a]

    last :: forall a. [a] -> Maybe a

    length :: forall a. [a] -> Prim.Number

    map :: forall a b. (a -> b) -> [a] -> [b]

    nub :: forall a. (Eq a) => [a] -> [a]

    nubBy :: forall a. (a -> a -> Prim.Boolean) -> [a] -> [a]

    null :: forall a. [a] -> Prim.Boolean

    range :: Prim.Number -> Prim.Number -> [Prim.Number]

    reverse :: forall a. [a] -> [a]

    singleton :: forall a. a -> [a]

    snoc :: forall a. [a] -> a -> [a]

    sort :: forall a. (Ord a) => [a] -> [a]

    tail :: forall a. [a] -> Maybe [a]

    take :: forall a. Prim.Number -> [a] -> [a]

    updateAt :: forall a. Prim.Number -> a -> [a] -> [a]

    zipWith :: forall a b c. (a -> b -> c) -> [a] -> [b] -> [c]


## Module Data.Array.Unsafe

### Values

    head :: forall a. [a] -> a

    tail :: forall a. [a] -> [a]