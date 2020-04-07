bimap
====

A data structure representing a bidirectional mapping between two key types.
Each value in the bimap is associated with exactly one value of the opposite type.

## Description about the forked version

Original Bimap is defined as `MkBimap !(Map a b) !(Map b a)`.
However, when using `String` like data for the key, this is not so efficient.
Therefore, `Data.Bimap.Hashable` changes one side of `Map` in `Bimap` as `HashMap`.
