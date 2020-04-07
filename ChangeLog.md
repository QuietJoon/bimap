Changelog for bimap
====

## Unreleased changes

### Add
* Add a new type of `Bimap` with `Hashable`

### Fix
* Make Stack compatible CI script (travis.yml)
* Add `ghc-prim` dependency with GHC version in `package.yaml`

## 0.3.1 -- 2015-10-17

### Added
* Added update and adjust functions (thanks to koral)

## 0.3.0 -- 2015-03-12

### Added
* Added map functions

## 0.2.4 -- 2008-08-25

### Added
* added filter and partition

### 0.2.3 -- 2008-07-01

### Added
* added fromAscPairList and fromAscPairListUnchecked
    (thanks to Janis Voigtländer)
* more tests for min/max functions (thanks to Jochem Berndsen)

## 0.2.2 2008-06-18

### Added
* added min/max functions (thanks to Jochem Berndsen)
* added tryInsert
* added fromAList
* more tests for existing functionality

## 0.2.1 2008-02-06

### Added
* added toMap and toMapR
* added big-O comments
* added "version" info in function comments

### Removed
* removed MTL dependency
* removed Control.Arrow dependency

### Changed
* now Haskell 98, modulo "foldl'" and hierarchical modules

## 0.2 2008-02-05

### Added
* Eq instance

### Changed
* large, incompatible interface overhaul
* GHC 6.8 support

## 0.1 2008-02-04

* initial release
* Data.Bimap and test suite
