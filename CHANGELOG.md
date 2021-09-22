# Changelog

Notable changes to this project are documented in this file. The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

Breaking changes:

New features:

Bugfixes:

Other improvements:
- Fixed minor documention issue with `find`

## [v6.0.1](https://github.com/purescript/purescript-arrays/releases/tag/v6.0.1) - 2021-04-19

Other improvements:
- Fixed warnings revealed by `v0.14.1` PS release (#213 by @JordanMartinez)

## [v6.0.0](https://github.com/purescript/purescript-arrays/releases/tag/v6.0.0) - 2021-02-26

Breaking changes:
- Added support for PureScript 0.14 and dropped support for all previous versions (#181)
- Renamed `Data.Array.ST.empty` to `Data.Array.ST.new` (#191, #198)
- Renamed `group'` to `groupAll` (#194, #200)

New features:
- Added specialized versions of the functions from `Data.Foldable` (#201):
  - Added `foldl`, `foldr`, `foldMap`, `fold`, `intercalate` to `Array`
  - Added `foldl1`, `foldr1`, `foldMap1`, `foldl1`, `intercalate` to `Array.NonEmpty`
- Added specialized `elem`, `notElem`, `find`, `findMap`, `scanl`, `scanr`, `any`, `all` (#189, #193, #201)
- Added `intersperse`, `groupAllBy`, `splitAt` (#179, #188, #194, #200, #201)
- Added `mapWithIndex`, `groupBy` to `Array.NonEmpty` (#201, #164)

Bugfixes:
- Fixed `sort`, so `undefined` is sorted by comparison function and not simply moved to the end of the array (#195, #197)

Other improvements:
- Generated changelog and added PR template (#208, #209)
- Added benchmarking (#178)
- Migrated to GitHub Actions for CI (#187, #169)
- Removed some internal usages of `unsafeCoerce` (#184)
- Changed `foldM` type signature to more closely match `foldl` (#160)
- Updated installation instructions to use Spago (#171)
- Replaced foreign `cons`, `snoc`, `drop`, `take` with PureScript implementations (#180)
- Removed `return {}` from FFI function for a small performance boost (#175)
- Bumped pulp version (#174)
- Removed primes from foreign modules exports (#168)

## [v5.3.1](https://github.com/purescript/purescript-arrays/releases/tag/v5.3.1) - 2019-10-13

Replace use of unsafeCoerce in freeze/thaw functions with discrete foreign functions (@andyarvanitis)

## [v5.3.0](https://github.com/purescript/purescript-arrays/releases/tag/v5.3.0) - 2019-04-27

Added `pop`, `shift`, `unshift`, `unshiftAll` for `Data.Array.ST` (@8084)

## [v5.2.1](https://github.com/purescript/purescript-arrays/releases/tag/v5.2.1) - 2019-03-31

Performance improvement for `nubByEq` (@sharno)

## [v5.2.0](https://github.com/purescript/purescript-arrays/releases/tag/v5.2.0) - 2018-12-16

- Added `run` function for `STArray` (@Dretch)

## [v5.1.1](https://github.com/purescript/purescript-arrays/releases/tag/v5.1.1) - 2018-12-02

Fixed issue with `fill` polyfill not being included in the bundle by `purs bundle` (@maximedenes, @zyla)

## [v5.1.0](https://github.com/purescript/purescript-arrays/releases/tag/v5.1.0) - 2018-09-25

* Make `groupBy` stable https://github.com/purescript/purescript-arrays/pull/148 (@LiamGoodacre)

## [v5.0.0](https://github.com/purescript/purescript-arrays/releases/tag/v5.0.0) - 2018-05-23

- Updated for PureScript 0.12
- Added `sort` functions for `STArray` (@matthewleon)
- `group` functions now return `NonEmptyArray` rather than `NonEmpty Array`s
- The `STArray` name prefixes have been dropped to reduce repetition when importing qualified
- Function argument order has been changed so that `STArray` is always the last argument
- `nub` (and related functions) now use `Ord` by default for a faster implementation, `Eq`-based variants are still available under new names

## [v4.4.0](https://github.com/purescript/purescript-arrays/releases/tag/v4.4.0) - 2018-04-26

- Added `Semigroup` instance for `NonEmptyArray`

## [v4.3.0](https://github.com/purescript/purescript-arrays/releases/tag/v4.3.0) - 2018-03-10

- Added `NonEmptyArray` (@matthewleon )

## [v4.2.2](https://github.com/purescript/purescript-arrays/releases/tag/v4.2.2) - 2017-12-16

* Add examples to the docs for most functions in `Data.Array` (@csicar)
* Remove some redundant parentheses (@matthewleon)

## [v4.2.1](https://github.com/purescript/purescript-arrays/releases/tag/v4.2.1) - 2017-10-01

- Preallocate result of `range` (@jacereda)

## [v4.2.0](https://github.com/purescript/purescript-arrays/releases/tag/v4.2.0) - 2017-09-05

Add `dropEnd` and `takeEnd` functions (@notgiorgi)

## [v4.1.2](https://github.com/purescript/purescript-arrays/releases/tag/v4.1.2) - 2017-06-25

Fix some bugs in `Data.Array.ST.Partial` (@mhuisi)

## [v4.1.1](https://github.com/purescript/purescript-arrays/releases/tag/v4.1.1) - 2017-06-20

* Improve performance of `unzip`; this function is now O(n) instead of O(n^2)
* Various documentation improvements

## [v4.1.0](https://github.com/purescript/purescript-arrays/releases/tag/v4.1.0) - 2017-05-28

- Batch update and modify functions (@matthewleon)
- Partial functions for ST arrays (@matthewleon)

## [v4.0.1](https://github.com/purescript/purescript-arrays/releases/tag/v4.0.1) - 2017-03-29

- Restored compiler-optimized TCO for `span`

## [v4.0.0](https://github.com/purescript/purescript-arrays/releases/tag/v4.0.0) - 2017-03-26

- Updated for PureScript 0.11.0
- `filterM` has been removed (`filterA` is better) (@hdgarrood)
- Improved performance of `concat` (@dikmax)
- Improved performance of `replicate` for browsers with `fill` (@dikmax)
- Added `sortWith` (@negator)

## [v3.2.1](https://github.com/purescript/purescript-arrays/releases/tag/v3.2.1) - 2017-02-14

Avoid `Discard` constraints in upcoming 0.11 release.

## [v3.2.0](https://github.com/purescript/purescript-arrays/releases/tag/v3.2.0) - 2017-01-20

- Add `Data.Array.ST.Iterator`, for iterating over things in ST computations
- Add `unsafeFreeze` for O(1) freezing of STArrays
- Use `~>` in the types for `toUnfoldable` and `fromFoldable` (@mlang)
- Performance boost for `groupBy`: was quadratic, now linear.

## [v3.1.0](https://github.com/purescript/purescript-arrays/releases/tag/v3.1.0) - 2016-11-24

- Significant performance boosts for the following functions:
  - `head` (now `O(1)`, was accidentally `O(n)`)
  - `toUnfoldable`
  - `span`
  - `difference`
- Add `filterA`, which is just like `filterM`, but faster, and it only requires you to have an `Applicative` (not necessarily a `Monad`).
- Deprecate `filterM` in favour of `filterA`. In the next major release, `filterM` will be removed.
- Add `unsnoc` (@joshuahhh).

## [v3.0.1](https://github.com/purescript/purescript-arrays/releases/tag/v3.0.1) - 2016-11-14

- Fixed shadowed name warning

## [v3.0.0](https://github.com/purescript/purescript-arrays/releases/tag/v3.0.0) - 2016-10-09

- The `group` functions now return `NonEmpty` groups

## [v2.0.0](https://github.com/purescript/purescript-arrays/releases/tag/v2.0.0) - 2016-10-07

- Updated dependencies
- `unsafeIndex` is exported from `Data.Array` rather than `Data.Array.Partial` now
- Array-specialised `replicate` is back
- Added stack safe version of `foldM` - `foldRecM` (@jutaro)
- Array now re-exports functions from `Foldable` and `Traversable` that might otherwise seem to be missing (@Risto-Stevcev)

## [v1.1.0](https://github.com/purescript/purescript-arrays/releases/tag/v1.1.0) - 2016-07-26

- Added `mapWithIndex` (@damncabbage)

## [v1.0.0](https://github.com/purescript/purescript-arrays/releases/tag/v1.0.0) - 2016-06-01

This release is intended for the PureScript 0.9.1 compiler and newer.

**Note**: The v1.0.0 tag is not meant to indicate the library is “finished”, the core libraries are all being bumped to this for the 0.9 compiler release so as to use semver more correctly.

## [v1.0.0-rc.7](https://github.com/purescript/purescript-arrays/releases/tag/v1.0.0-rc.7) - 2016-05-20

- Fixed warning for unused FFI implementation

## [v1.0.0-rc.6](https://github.com/purescript/purescript-arrays/releases/tag/v1.0.0-rc.6) - 2016-05-20

- Removed `replicate` and `replicateM` as these are provided via the unfoldable instance now

## [v1.0.0-rc.5](https://github.com/purescript/purescript-arrays/releases/tag/v1.0.0-rc.5) - 2016-05-20

- Fixed unused import warning

## [v1.0.0-rc.4](https://github.com/purescript/purescript-arrays/releases/tag/v1.0.0-rc.4) - 2016-04-04

- Added `toUnfoldable`

## [v1.0.0-rc.3](https://github.com/purescript/purescript-arrays/releases/tag/v1.0.0-rc.3) - 2016-03-27

- Renamed `Data.Array.Unsafe` to `Data.Array.Partial` and added `Partial` constraint for parity with `purescript-lists`.

## [v1.0.0-rc.2](https://github.com/purescript/purescript-arrays/releases/tag/v1.0.0-rc.2) - 2016-03-17

- Added `fromFoldable` (@hdgarrood)

## [v1.0.0-rc.1](https://github.com/purescript/purescript-arrays/releases/tag/v1.0.0-rc.1) - 2016-03-16

- Release candidate for the psc 0.8+ core libraries

## [v0.4.5](https://github.com/purescript/purescript-arrays/releases/tag/v0.4.5) - 2016-02-27

- Added `partition` (@raichoo)

## [v0.4.4](https://github.com/purescript/purescript-arrays/releases/tag/v0.4.4) - 2015-12-11

- Document instances for Pursuit (@hdgarrood)

## [v0.4.3](https://github.com/purescript/purescript-arrays/releases/tag/v0.4.3) - 2015-11-02

- Removed unused imports

## [v0.4.2](https://github.com/purescript/purescript-arrays/releases/tag/v0.4.2) - 2015-08-13

- Fixed warnings about partial functions

## [v0.4.1](https://github.com/purescript/purescript-arrays/releases/tag/v0.4.1) - 2015-07-29

- `replicateM` is now stack safe (@hdgarrood)

## [v0.4.0](https://github.com/purescript/purescript-arrays/releases/tag/v0.4.0) - 2015-06-30

This release works with versions 0.7.\* of the PureScript compiler. It will not work with older versions. If you are using an older version, you should require an older, compatible version of this library.

## [v0.4.0-rc.2](https://github.com/purescript/purescript-arrays/releases/tag/v0.4.0-rc.2) - 2015-06-07

Updates for parity with `purescript-lists`:
- Added `insert`, `insertBy`, `alterAt`, `union`, `unionBy`
- The `insertAt`, `modifyAt`, `deleteAt`... functions now return `Nothing` when given an out of range index

## [v0.4.0-rc.1](https://github.com/purescript/purescript-arrays/releases/tag/v0.4.0-rc.1) - 2015-06-06

Initial release candidate of the library intended for the 0.7 compiler.

## [v0.3.7](https://github.com/purescript/purescript-arrays/releases/tag/v0.3.7) - 2015-04-01

Add `replicate` (@jacereda)

## [v0.3.6](https://github.com/purescript/purescript-arrays/releases/tag/v0.3.6) - 2015-03-24

Fix `pokeSTArray` bounds check (@jacereda)

## [v0.3.5](https://github.com/purescript/purescript-arrays/releases/tag/v0.3.5) - 2015-03-18

Improve complexity of `head` and `last` (@hdgarrood)

## [v0.3.4](https://github.com/purescript/purescript-arrays/releases/tag/v0.3.4) - 2015-03-17

Update docs

## [v0.3.3](https://github.com/purescript/purescript-arrays/releases/tag/v0.3.3) - 2015-03-08

Add `modifyAt`.

## [v0.3.2](https://github.com/purescript/purescript-arrays/releases/tag/v0.3.2) - 2015-02-18



## [v0.3.1](https://github.com/purescript/purescript-arrays/releases/tag/v0.3.1) - 2015-01-24

Add `take` and `drop`.

## [v0.3.0](https://github.com/purescript/purescript-arrays/releases/tag/v0.3.0) - 2014-11-28

Add new `ST` functions.

## [v0.2.1](https://github.com/purescript/purescript-arrays/releases/tag/v0.2.1) - 2014-08-23

Include `(..)` operator.

## [v0.2.0](https://github.com/purescript/purescript-arrays/releases/tag/v0.2.0) - 2014-08-11

- Add `Alt`, `Plus`, `MonadPlus`, update `Alternative` (@garyb)

## [v0.1.8](https://github.com/purescript/purescript-arrays/releases/tag/v0.1.8) - 2014-05-30



## [v0.1.7](https://github.com/purescript/purescript-arrays/releases/tag/v0.1.7) - 2014-05-29



## [v0.1.6](https://github.com/purescript/purescript-arrays/releases/tag/v0.1.6) - 2014-05-22

- Added `delete`, `deleteBy`, `(\\)` (garyb)
- Added `intersect`, `intersectBy`, and `last` in `Data.Array.Unsafe` (paf31)

## [v0.1.5](https://github.com/purescript/purescript-arrays/releases/tag/v0.1.5) - 2014-05-22

- Added `group`, `groupBy`, `span` (joneshf)
- Added `catMaybes` (garyb)

## [v0.1.4](https://github.com/purescript/purescript-arrays/releases/tag/v0.1.4) - 2014-05-08

- Added `sortBy` (joneshf)

## [v0.1.3](https://github.com/purescript/purescript-arrays/releases/tag/v0.1.3) - 2014-05-05

- Removed `elem` as it is implemented in `Data.Foldable`
- Updated `elemIndex` and `elemLastIndex` to use `Eq` when finding items.
- Added `findIndex` and `findLastIndex` to find an item with a predicate.

## [v0.1.2](https://github.com/purescript/purescript-arrays/releases/tag/v0.1.2) - 2014-05-03

Added `mapMaybe`

## [v0.1.1](https://github.com/purescript/purescript-arrays/releases/tag/v0.1.1) - 2014-04-27



## [v0.1.0](https://github.com/purescript/purescript-arrays/releases/tag/v0.1.0) - 2014-04-25
