{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE TypeFamilies #-}
#endif

{-| FIXME: Update descriptions
An implementation of bidirectional maps between values of two
key types. A 'HMBimap' is essentially a bijection between subsets of
its two argument types.

Each element of the left-hand type is associated with an element
of the right-hand type, and vice-versa, such that the two mappings
are inverses. Deleting an element will cause its twin to be deleted,
and inserting a pair of elements will cause any overlapping bindings
to be deleted.

Most functions implicitly consider the left-hand type to be the
key, and the right-hand type to be the value.
Functions with an @R@ suffix reverse this convention, treating the
right-hand type as the key and the left-hand type as the value.
-}
module Data.Bimap.Hashable.HMBimap (
    -- * Bimap type
    HMBimap(),
    -- * Query
    null,
    size,
    member,
    memberR,
    notMember,
    notMemberR,
    pairMember,
    pairNotMember,
    lookup,
    lookupR,
    (!),
    (!>),
    -- * Construction
    empty,
    singleton,
    -- * Update
    insert,
    tryInsert,
    --adjust,
    --adjustR,
    --adjustWithKey,
    --adjustWithKeyR,
    --update,
    --updateR,
    --updateWithKey,
    --updateWithKeyR,
    delete,
    deleteR,
    -- * Min\/Max
    --findMin,
    findMinR,
    --findMax,
    findMaxR,
    --deleteMin,
    --deleteMinR,
    --deleteMax,
    --deleteMaxR,
    --deleteFindMin,
    --deleteFindMinR,
    --deleteFindMax,
    --deleteFindMaxR,
    -- * Filter
    filter,
    --partition,
    -- * Conversion\/traversal
    fromList,
    fromAList,
    fromAscPairList,
    fromAscPairListUnchecked,
    toList,
    toAscList,
    --toAscListR,
    keys,
    keysR,
    elems,
    assocs,
    fold,
    --Data.Bimap.Hashable.map,
    mapR,
    --mapMonotonic,
    mapMonotonicR,
    --toMap,
    toMapR,
    -- * Miscellaneous
    --valid,
    --twist,
    --twisted,
) where

import           Control.DeepSeq     (NFData)
import           Control.Monad.Catch

import           Data.Function       (on)
import           Data.List           (foldl', sort)
import qualified Data.Hashable       as H
import qualified Data.HashMap.Strict as H
import qualified Data.Map            as M
import           Data.Maybe          (fromMaybe)
import           Data.Typeable

#if __GLASGOW_HASKELL__ >= 708
import qualified GHC.Exts            as GHCExts
#endif
import           GHC.Generics        (Generic)

import           Prelude             hiding (filter, lookup, null, pred)
import qualified Prelude             as P


infixr 9 .:
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.).(.)

{-|
A bidirectional map between values of types Hashable @a@ and @b@.
-}
data HMBimap a b = MkHMBimap !(H.HashMap a b) !(M.Map b a) deriving (Generic)

instance (Show a, Show b) => Show (HMBimap a b) where
    show x = "fromList " ++ (show . toList $ x)

instance (Eq a, Eq b) => Eq (HMBimap a b) where
    (==) = (==) `on` toAscList

instance (Ord a, Ord b) => Ord (HMBimap a b) where
    compare = compare `on` toAscList

instance (NFData a, NFData b) => NFData (HMBimap a b)

#if __GLASGOW_HASKELL__ >= 708
instance (H.Hashable a, Ord a, Ord b) => GHCExts.IsList (HMBimap a b) where
    type Item (HMBimap a b) = (a, b)
    fromList = fromList
    toList = toList
#endif

{-|
A 'HMBimap' action failed.
-}
data HMBimapException = KeyNotFound String
  deriving(Eq, Show, Typeable)

instance Exception HMBimapException

{-| /O(1)/. The empty bimap.
/Version: 0.2/-}
empty :: HMBimap a b
empty = MkHMBimap H.empty M.empty

{-| /O(1)/. A bimap with a single element.
/Version: 0.2/-}
singleton :: H.Hashable a => a -> b -> HMBimap a b
singleton x y = MkHMBimap (H.singleton x y) (M.singleton y x)

{-| /O(1)/. Is the bimap empty?
/Version: 0.2/-}
null :: HMBimap a b -> Bool
null (MkHMBimap left _) = H.null left

{-| /O(1)/. The number of elements in the bimap.
/Version: 0.2/-}
size :: HMBimap a b -> Int
size (MkHMBimap left _) = H.size left

{-| /O(log n)/. Is the specified value a member of the bimap?
/Version: 0.2/-}
member :: (H.Hashable a, Ord a, Ord b) => a -> HMBimap a b -> Bool
member x (MkHMBimap left _) = H.member x left
{-| /O(log n)/. A version of 'member' specialized to the right key.
/Version: 0.2/-}
memberR :: (Ord a, Ord b) => b -> HMBimap a b -> Bool
memberR y (MkHMBimap _ right) = M.member y right

{-| /O(log n)/. Is the specified value not a member of the bimap?
/Version: 0.2/-}
notMember :: (H.Hashable a, Ord a, Ord b) => a -> HMBimap a b -> Bool
notMember = not .: member
{-| /O(log n)/. A version of 'notMember' specialized to the right key.
/Version: 0.2/-}
notMemberR :: (Ord a, Ord b) => b -> HMBimap a b -> Bool
notMemberR = not .: memberR

{-| /O(log n)/.
Are the two values associated /with each other/ in the bimap?

This function is uncurried in its first two arguments, so that it
can be used infix.

/Version: 0.2/-}
pairMember :: (H.Hashable a, Ord a, Ord b)
           => (a, b) -> HMBimap a b -> Bool
pairMember (x, y) (MkHMBimap left _) =
    maybe False (== y) (H.lookup x left)

{-| /O(log n)/.
Are the two values not in the bimap, or not associated
with each other? (Complement of 'pairMember'.)
/Version: 0.2/-}
pairNotMember :: (H.Hashable a, Ord a, Ord b)
              => (a, b) -> HMBimap a b -> Bool
pairNotMember = not .: pairMember

{-| /O(log n)/.
Insert a pair of values into the bimap, associating them.

If either of the values is already in the bimap, any overlapping
bindings are deleted.

/Version: 0.2/-}
insert :: (H.Hashable a, Ord a, Ord b)
       => a -> b -> HMBimap a b -> HMBimap a b
insert x y = delete x >>> deleteR y >>> unsafeInsert x y
    where
    (>>>) = flip (.)

{-| /O(log n)/.
Insert a pair of values into the bimap, but only if neither is
already in the bimap.
/Version: 0.2.2/-}
tryInsert :: (H.Hashable a, Ord a, Ord b)
          => a -> b -> HMBimap a b -> HMBimap a b
tryInsert x y bi
    | x `notMember` bi && y `notMemberR` bi = unsafeInsert x y bi
    | otherwise                               = bi

{-| /O(log n)/.
Insert a pair of values into the bimap, without checking for
overlapping bindings.

If either value is already in the bimap, and
is not bound to the other value, the bimap will become inconsistent.
-}
unsafeInsert :: (H.Hashable a, Ord a, Ord b)
             => a -> b -> HMBimap a b -> HMBimap a b
unsafeInsert x y (MkHMBimap left right) =
    MkHMBimap (H.insert x y left) (M.insert y x right)

{-| /O(log n)/. Common implementation for 'delete' and 'deleteR'. -}
deleteE :: (H.Hashable a, Ord a, Ord b)
       => Either a b -> HMBimap a b -> HMBimap a b
deleteE e (MkHMBimap left right) =
    MkHMBimap
        (perhaps H.delete x  left)
        (perhaps M.delete y  right)
    where
    perhaps = maybe id
    x = either Just (`M.lookup` right) e
    y = either (`H.lookup` left) Just  e

{-| /O(log n)/.
Delete a value and its twin from a bimap.

When the value is not a member of the bimap, the original bimap is
returned.

/Version: 0.2/-}
delete :: (H.Hashable a, Ord a, Ord b) => a -> HMBimap a b -> HMBimap a b
delete = deleteE . Left

{-| /O(log n)/ A version of 'delete' specialized to the right key.
/Version: 0.2/-}
deleteR :: (H.Hashable a, Ord a, Ord b) => b -> HMBimap a b -> HMBimap a b
deleteR = deleteE . Right

{-| /O(log n)/.
Update a value at a specific left key with the result of the provided function.

When the left key is not a member of the bimap, the original bimap is returned.-}
{- FIXME: Not yet implemented correctly
adjust :: (H.Hashable a, Ord a, Ord b) => (b -> b) -> a -> HMBimap a b -> HMBimap a b
adjust f = adjustWithKey (const f)
-}

{-| /O(log n)/.
Update a value at a specific right key with the result of the provided function.

When the right key is not a member of the bimap, the original bimap is returned.-}
{- FIXME: Not yet implemented correctly
adjustR :: (Ord a, Ord b) => (a -> a) -> b -> HMBimap a b -> HMBimap a b
adjustR f b = reverseHMBimap . adjust f b . reverseHMBimap
  where reverseHMBimap (MkHMBimap left right) = MkHMBimap right left
-}

{-| /O(log n)/.
Adjust a value at a specific left key.

When the left key is not a member of the bimap, the original bimap is returned.-}
{- FIXME: Not yet implemented correctly
adjustWithKey :: (H.Hashable a, Ord a, Ord b) => (a -> b -> b) -> a -> HMBimap a b -> HMBimap a b
adjustWithKey f = updateWithKey (\a -> Just . f a)
-}

{-| /O(log n)/.
Adjust a value at a specific right key.

When the right key is not a member of the bimap, the original bimap is returned.-}
{- FIXME: Not yet implemented correctly
adjustWithKeyR :: (Ord a, Ord b) => (b -> a -> a) -> b -> HMBimap a b -> HMBimap a b
adjustWithKeyR f b = reverseHMBimap . adjustWithKey f b . reverseHMBimap
  where reverseHMBimap (MkHMBimap left right) = MkHMBimap right left
-}

{-| /O(log n)/.
The expression (@'update' f a bimap@) updates the right value @b@ at @a@ (if it is in the bimap).

If (@f b@) is 'Nothing', the element is deleted.

If it is (@'Just' y@), the left key @a@ is bound to the new value @y@.-}
{- FIXME: Not yet implemented correctly
update :: (H.Hashable a, Ord a, Ord b) => (b -> Maybe b) -> a -> HMBimap a b -> HMBimap a b
update f = updateWithKey (const f)
-}

{-| /O(log n)/.
The expression (@'updateR' f b bimap@) updates the left value @a@ at @b@ (if it is in the bimap).

If (@f a@) is 'Nothing', the element is deleted.

If it is (@'Just' x@), the right key @b@ is bound to the new value @x@.-}
{- FIXME: Not yet implemented correctly
updateR :: (Ord a, Ord b) => (a -> Maybe a) -> b -> HMBimap a b -> HMBimap a b
updateR f b = reverseHMBimap . update f b . reverseHMBimap
  where reverseHMBimap (MkHMBimap left right) = MkHMBimap right left
-}

{-| /O(log n)/.
The expression (@'updateWithKey' f a bimap@) updates the right value @b@ at @a@ (if it is in the bimap).

If (@f a b@) is 'Nothing', the element is deleted.

If it is (@'Just' y@), the left key @a@ is bound to the new value @y@.-}
{- FIXME: Not yet implemented correctly
updateWithKey :: (H.Hashable a, Ord a, Ord b) => (a -> b -> Maybe b) -> a -> HMBimap a b -> HMBimap a b
updateWithKey f a (MkHMBimap left right) = MkHMBimap left' right' where
  oldB = H.lookup a left
  newB = f a =<< oldB
  oldA = newB >>= (`M.lookup` right) >>= \x -> if x == a then Nothing else Just x
  left' = maybe id H.delete oldA $ M.updateWithKey f a left
  right' = maybe id (`M.insert` a) newB $ maybe id M.delete oldB right
-}
{-| /O(log n)/.
The expression (@'updateWithKeyR' f b bimap@) updates the left value @a@ at @b@ (if it is in the bimap).

If (@f b a@) is 'Nothing', the element is deleted.

If it is (@'Just' x@), the right key @b@ is bound to the new value @x@.-}
{- FIXME: Not yet implemented correctly
updateWithKeyR :: (Ord a, Ord b) => (b -> a -> Maybe a) -> b -> HMBimap a b -> HMBimap a b
updateWithKeyR f b = reverseHMBimap . updateWithKey f b . reverseHMBimap
  where reverseHMBimap (MkHMBimap left right) = MkHMBimap right left
-}

{-| /O(log n)/.
Lookup a left key in the bimap, returning the associated right key.

This function will @return@ the result in the monad, or @fail@ if
the value isn't in the bimap.

/Version: 0.2/-}
lookup :: (H.Hashable a, Ord a, Ord b, MonadThrow m)
       => a -> HMBimap a b -> m b
lookup x (MkHMBimap left _) =
    maybe (throwM $ KeyNotFound "Data.Bimap.Hashable.lookup")
          return
          (H.lookup x left)

{-| /O(log n)/.
A version of 'lookup' that is specialized to the right key,
and returns the corresponding left key.
/Version: 0.2/-}
lookupR :: (Ord a, Ord b, MonadThrow m)
        => b -> HMBimap a b -> m a
lookupR y (MkHMBimap _ right) =
    maybe (throwM $ KeyNotFound "Data.Bimap.Hashable.lookupR")
          return
          (M.lookup y right)

{-| /O(log n)/.
Find the right key corresponding to a given left key.
Calls @'error'@ when the key is not in the bimap.
/Version: 0.2/-}
(!) :: (H.Hashable a, Ord a, Ord b) => HMBimap a b -> a -> b
(!) bi x = fromMaybe (error "Data.Bimap.Hashable.(!): Left key not found") $ lookup x bi

{-| /O(log n)/.
A version of @(!)@ that is specialized to the right key,
and returns the corresponding left key.
/Version: 0.2/-}
(!>) :: (Ord a, Ord b) => HMBimap a b -> b -> a
(!>) bi y = fromMaybe (error "Data.Bimap.Hashable.(!>): Right key not found") $ lookupR y bi

{-| /O(n*log n)/.
Build a map from a list of pairs. If there are any overlapping
pairs in the list, the later ones will override the earlier ones.
/Version: 0.2/-}
fromList :: (H.Hashable a, Ord a, Ord b)
         => [(a, b)] -> HMBimap a b
fromList = foldl' (flip . uncurry $ insert) empty

{-| /O(n*log n)/.
Build a map from a list of pairs. Unlike 'fromList', earlier pairs
will take precedence over later ones.

The name @fromAList@ is a reference to Lisp-style association
lists, where associations can be overridden by prepending new ones.

Note that when duplicates occur in both the keys and in the values,
@fromList xs /= fromAList (reverse xs)@. However, if either
contains no duplicates, then the equality holds.

/Version: 0.2.2/-}
fromAList :: (H.Hashable a, Ord a, Ord b)
          => [(a, b)] -> HMBimap a b
fromAList = foldl' (flip . uncurry $ tryInsert) empty

{-| /O(n)/. Convert to a list of associated pairs.
/Version: 0.2/-}
toList :: HMBimap a b -> [(a, b)]
toList = toAscList

{-| /O(n)/. Build a bimap from a list of pairs, where both the @fst@
and @snd@ halves of the list are in strictly ascending order.

This precondition is checked; an invalid list will cause an error.

/Version: 0.2.3/-}
fromAscPairList :: (H.Hashable a, Ord a, Ord b)
                => [(a, b)] -> HMBimap a b
fromAscPairList xs
    | isBiAscending xs = fromAscPairListUnchecked xs
    | otherwise        = error
        "Data.Bimap.Hashable.fromAscPairList: list not correctly ascending"

isBiAscending :: (Ord a, Ord b)
              => [(a, b)] -> Bool
isBiAscending = allAdjacent bothLess
    where
    -- True if the binary relation f is true for all adjacent pairs
    -- in the input list
    allAdjacent :: (c -> c -> Bool) -> [c] -> Bool
    allAdjacent f xs = all (uncurry f) $ zip xs (tail xs)
    -- True if both components of the first pair are strictly less
    -- than their counterparts in the second pair
    bothLess (x1, y1) (x2, y2) = (x1 < x2) && (y1 < y2)

{-| /O(n)/. Build a bimap from a list of pairs, where both the @fst@
and @snd@ halves of the list are in strictly ascending order.

This precondition is /not/ checked; an invalid list will produce a
malformed bimap.

/Version: 0.2.3/-}
fromAscPairListUnchecked :: (H.Hashable a, Ord a, Ord b)
                         => [(a, b)] -> HMBimap a b
fromAscPairListUnchecked xs = MkHMBimap
    (H.fromList xs)
    (M.fromAscList $ P.map swap  xs)
    where
    swap (x, y) = (y, x)

{-| /O(n)/.
Convert to a list of associated pairs, with the left-hand
values in ascending order.

Since pair ordering is lexical, the pairs will also be in
ascending order.

/Version: 0.2/-}
toAscList :: HMBimap a b -> [(a, b)]
toAscList (MkHMBimap left _) = H.toList left

{-| /O(n)/.
Convert to a list of associated pairs, with the right-hand
values first in the pair and in ascending order.

Since pair ordering is lexical, the pairs will also be in
ascending order.

/Version: 0.2/-}
{- FIXME: Not yet implemented correctly
toAscListR :: HMBimap a b -> [(b, a)]
toAscListR = toAscList . twist
-}

{-| /O(n)/.
Return all associated pairs in the bimap, with the left-hand
values in ascending order.
/Version: 0.2/-}
assocs :: HMBimap a b -> [(a, b)]
assocs = toList

{-| /O(n)/.
Return all left-hand keys in the bimap in ascending order.
/Version: 0.2/-}
keys :: HMBimap a b -> [a]
keys (MkHMBimap left _) = H.keys left

{-| /O(n)/.
Return all right-hand keys in the bimap in ascending order.
/Version: 0.2/-}
keysR :: HMBimap a b -> [b]
keysR (MkHMBimap _ right) = M.keys right

{-| /O(n)/. An alias for 'keysR'.
/Version: 0.2/-}
elems :: HMBimap a b -> [b]
elems = keysR

{-| /O(1)/. Extract only the left-to-right component of a bimap.
/Version: 0.2.1/-}
{- FIXME: Not yet implemented correctly
toMap :: HMBimap a b -> M.Map a b
toMap (MkHMBimap left _) = left
-}

{-| /O(1)/. Extract only the right-to-left component of a bimap.
/Version: 0.2.1/-}
toMapR :: HMBimap a b -> M.Map b a
toMapR (MkHMBimap _ right) = right

{-| /O(n)/.
Filter all association pairs that satisfy the predicate.

Note that the predicate will be applied /twice/ for each association
in the bimap.

/Version: 0.2.4/-}
filter :: (H.Hashable a, Ord a, Ord b)
              => (a -> b -> Bool) -> HMBimap a b -> HMBimap a b
filter pred (MkHMBimap left right) =
    MkHMBimap
        (H.filterWithKey pred left)
        (M.filterWithKey (flip pred) right)

{-| /O(n)/.
Partition the bimap according to a predicate.
The first bimap contains all associations that satisfy the predicate;
the second contains all associations that fail the predicate.

Note that the predicate will be applied /twice/ for each association
in the bimap.

/Version: 0.2.4/-}
{- FIXME: Not yet implemented correctly
partition :: (Ord a, Ord b)
          => (a -> b -> Bool) -> HMBimap a b -> (HMBimap a b, HMBimap a b)
partition pred (MkHMBimap left right) =
    (,) (MkHMBimap leftA rightA) (MkHMBimap leftB rightB)
    where
    (leftA, leftB) = M.partitionWithKey pred left
    (rightA, rightB) = M.partitionWithKey (flip pred) right
-}


{-| /O(n*log n)/.
Test if the internal bimap structure is valid. This should be true
for any bimap created using the public interface, unless
'fromAscPairListUnchecked' has been used inappropriately.
/Version: 0.2/-}
{- FIXME: Not yet implemented correctly
valid :: (Ord a, Ord b)
      => HMBimap a b -> Bool
valid (MkHMBimap left right) = and
    [ M.valid left, M.valid right
    , (==)
        (sort .                  H.toList $ left )
        (sort . P.map flipPair . M.toList $ right)
    ]
    where
    flipPair (x, y) = (y, x)
-}

{-| /O(1)/.
Reverse the positions of the two element types in the bimap.
/Version: 0.2/-}
{- FIXME: Just delete this
twist ::  HMBimap a b -> HMBimap b a
twist (MkHMBimap left right) = MkHMBimap right left
-}

{-| /O(1)/.
Reverse the positions of the two element types in a bimap
transformation.
/Version: 0.2/-}
{- FIXME: Just delete this
twisted :: (HMBimap a b -> HMBimap a b) -> (HMBimap b a -> HMBimap b a)
twisted f = twist . f . twist
-}

{-| /O(n)/.
Fold the association pairs in the map, such that
@'fold' f z == 'foldr' f z . 'assocs'@.
/Version: 0.2/-}
fold :: (a -> b -> c -> c) -> c -> HMBimap a b -> c
fold f z = foldr (uncurry f) z . assocs

{-| /O(n*log n)/
Map a function over all the left keys in the map.
/Version 0.3/-}
{- FIXME: Just delete this
map :: Ord c => (a -> c) -> HMBimap a b -> HMBimap c b
map f (MkHMBimap left right) =
    MkHMBimap (H.mapKeys f left) (M.map f right)
    -}

{-| /O(n*log n)/
Map a function over all the right keys in the map.
/Version 0.3/-}
mapR :: Ord c => (b -> c) -> HMBimap a b -> HMBimap a c
mapR f (MkHMBimap left right) =
    MkHMBimap (H.map f left) (M.mapKeys f right)

{-| /O(n)/.
Map a strictly increasing function over all left keys in the map.
/The precondition is not checked./
/Version 0.3/-}
{- FIXME: Just delete this
mapMonotonic :: (a -> c) -> HMBimap a b -> HMBimap c b
mapMonotonic f (MkHMBimap left right) =
    MkHMBimap (M.mapKeysMonotonic f left) (M.map f right)
    -}

{-| /O(n)/.
Map a strictly increasing function over all right keys in the map.
/The precondition is not checked./
/Version 0.3/-}
mapMonotonicR :: (b -> c) -> HMBimap a b -> HMBimap a c
mapMonotonicR f (MkHMBimap left right) =
    MkHMBimap (H.map f left) (M.mapKeysMonotonic f right)

{-| /O(log n)/.
Delete and find the element with maximal left key.
Calls @'error'@ if the bimap is empty.
/Version: 0.2.2/-}
{- FIXME: Just delete this
deleteFindMax :: (Ord b) => HMBimap a b -> ((a, b), HMBimap a b)
deleteFindMax (MkHMBimap left right) = ((a, b), MkHMBimap left' right') where
    ((a, b), left') = M.deleteFindMax left
    right' = b `M.delete` right
-}

{-| /O(log n)/.
Delete and find the element with maximal right key.
Calls @'error'@ if the bimap is empty.
/Version: 0.2.2/-}
{- FIXME: Just delete this}
deleteFindMaxR :: (Ord a) => HMBimap a b ->  ((b, a), HMBimap a b)
deleteFindMaxR = second twist . deleteFindMax . twist where
    second f (x, y) = (x, f y)
-}

{-| /O(log n)/.
Delete the element with maximal left key.
Calls @'error'@ if the bimap is empty.
/Version: 0.2.2/-}
{- FIXME: Just delete this
deleteMax :: (Ord b) => HMBimap a b -> HMBimap a b
deleteMax = snd . deleteFindMax
-}

{-| /O(log n)/.
Delete the element with maximal right key.
Calls @'error'@ if the bimap is empty.
/Version: 0.2.2/-}
{- FIXME: Not yet implemented correctly
deleteMaxR :: (Ord a) => HMBimap a b -> HMBimap a b
deleteMaxR = snd . deleteFindMaxR
-}

{-| /O(log n)/.
Find the element with maximal left key.
Calls @'error'@ if the bimap is empty.
/Version: 0.2.2/-}
{- FIXME: Just delete this
findMax :: HMBimap a b -> (a, b)
findMax = M.findMax . toMap
-}

{-| /O(log n)/.
Find the element with maximal right key. The
right-hand key is the first entry in the pair.
Calls @'error'@ if the bimap is empty.
/Version: 0.2.2/-}
findMaxR :: HMBimap a b -> (b, a)
findMaxR = M.findMax . toMapR

{-| /O(log n)/.
Delete and find the element with minimal left key.
Calls @'error'@ if the bimap is empty.
/Version: 0.2.2/-}
{- FIXME: Just delete it
deleteFindMin :: (Ord b) => HMBimap a b -> ((a, b), HMBimap a b)
deleteFindMin (MkHMBimap left right) = ((a, b), MkHMBimap left' right') where
    ((a, b), left') = M.deleteFindMin left
    right' = b `M.delete` right
-}

{-| /O(log n)/.
Delete and find the element with minimal right key.
Calls @'error'@ if the bimap is empty.
/Version: 0.2.2/-}
{- FIXME: Just delete this
deleteFindMinR :: (Ord a) => HMBimap a b ->  ((b, a), HMBimap a b)
deleteFindMinR = second twist . deleteFindMin . twist where
    second f (x, y) = (x, f y)
-}

{-| /O(log n)/.
Delete the element with minimal left key.
Calls @'error'@ if the bimap is empty.
/Version: 0.2.2/-}
{- FIXME: Just delete this
deleteMin :: (Ord b) => HMBimap a b -> HMBimap a b
deleteMin = snd . deleteFindMin
-}

{-| /O(log n)/.
Delete the element with minimal right key.
Calls @'error'@ if the bimap is empty.
/Version: 0.2.2/-}
{- FIXME: Not yet implemented correctly
deleteMinR :: (Ord a) => HMBimap a b -> HMBimap a b
deleteMinR = snd . deleteFindMinR
-}

{-| /O(log n)/.
Find the element with minimal left key.
Calls @'error'@ if the bimap is empty.
/Version: 0.2.2/-}
{- FIXME: Just delete this
findMin :: HMBimap a b -> (a, b)
findMin = M.findMin . toMap
-}

{-| /O(log n)/.
Find the element with minimal right key. The
right-hand key is the first entry in the pair.
Calls @'error'@ if the bimap is empty.
/Version: 0.2.2/-}
findMinR :: HMBimap a b -> (b, a)
findMinR = M.findMin . toMapR
