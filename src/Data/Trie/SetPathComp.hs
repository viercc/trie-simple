{-# LANGUAGE DeriveTraversable #-}
module Data.Trie.SetPathComp(
  -- * Types
  TSet(..),
  -- * Queries
  member, notMember,
  beginWith,
  null, count, enumerate,
  foldr, foldMap, foldl',
  -- * Construction
  empty, epsilon,
  singleton,
  insert, delete,
  -- * Combine
  union, intersection, difference,
  --append,
  -- * Other operations
  --prefixes, suffixes, infixes,
  -- * Conversion
  fromList, toList,
  fromAscList, toAscList,
  fromSet, toSet,
  -- * Low-level operation
  Node(..),
  foldTSet, paraTSet
)
where

import Prelude hiding (foldMap, foldr, null)

import           Control.Applicative hiding (empty)
import qualified Control.Applicative as Ap

import           Data.Semigroup
import qualified Data.Foldable   as F
import           Data.Function   (on)
import qualified Data.List       as List (foldr, foldl')
import           Data.Maybe      (fromMaybe)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Control.Arrow ((&&&))

import qualified Data.Vector as V

import Control.DeepSeq

data Node c r = Node !(V.Vector c) !Bool !(Map c r)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

instance (NFData c, NFData r) => NFData (Node c r) where
  rnf (Node cs a e) = rnf cs `seq` rnf a `seq` rnf e

newtype TSet c = TSet { getNode :: Node c (TSet c) }

-- TODO: improve Eq and Ord instance
instance Eq c => Eq (TSet c) where
  (==) = (==) `on` enumerate

instance Ord c => Ord (TSet c) where
  compare = compare `on` enumerate

instance Show c => Show (TSet c) where
  showsPrec p t = showParen (p > 10) $
    showString "fromList " . showsPrec 11 (enumerate t)

instance (NFData c) => NFData (TSet c) where
  rnf (TSet node) = rnf node

{-

The canonical Monoid instance could be (epsilon, append),
but here I choose (empty, union) to align to Set instance.
Semigroup instance must follow how Monoid is defined.

-}

-- | Semigroup(union)
instance (Ord c) => Semigroup (TSet c) where
  (<>) = union
  stimes = stimesIdempotent

-- | Monoid(empty, union)
instance (Ord c) => Monoid (TSet c) where
  mempty = empty
  mappend = (<>)

-- * Queries
member :: (Ord c) => [c] -> TSet c -> Bool
member cs (TSet (Node ds a e1)) = loop cs ds 0 a e1
  where
    loop cs ds n a e1
      | V.length ds > n  = case cs of
          []    -> False
          c:cs' -> c == ds V.! n && loop cs' ds (n+1) a e1
      | V.length ds == n = case cs of
          []    -> a
          c:cs' -> case Map.lookup c e1 of
            Nothing -> False
            Just t' -> member cs' t'
      | otherwise = error "Never come here!"
          

notMember :: (Ord c) => [c] -> TSet c -> Bool
notMember cs = not . member cs

-- | @beginWith t xs@ returns new TSet @t'@ which contains
--   all string @ys@ such that @t@ contains @xs ++ ys@.
beginWith :: (Ord c) => TSet c -> [c] -> TSet c
beginWith (TSet (Node ds a e1)) cs = loop cs ds 0 a e1
  where
    loop cs ds n a e1
      | V.length ds > n  = case cs of
          []    -> TSet $ Node (V.drop n ds) a e1
          c:cs' -> if c == ds V.! n
                     then loop cs' ds (n+1) a e1
                     else empty
      | V.length ds == n = case cs of
          []    -> TSet $ Node V.empty a e1
          c:cs' -> case Map.lookup c e1 of
            Nothing -> empty
            Just t' -> beginWith t' cs'
      | otherwise = error "Never come here!"

null :: TSet c -> Bool
null (TSet (Node _ a e)) = not a && Map.null e

-- | Returns number of elements. @count@ takes O(number of nodes)
--   unlike 'Set.size' which is O(1).
count :: TSet c -> Int
count = foldTSet count'
  where
    count' (Node _ a e) =
      (if a then 1 else 0) + sum e

-- | List of all elements.
enumerate :: TSet c -> [[c]]
enumerate = foldr (:) []

{-
from this post by u/foBrowsing:
  https://www.reddit.com/r/haskell/comments/8krv31/how_to_traverse_a_trie/dzaktkn/
-}
foldr :: ([c] -> r -> r) -> r -> TSet c -> r
foldr f z (TSet (Node ds a e))
  | a         = f' [] r
  | otherwise = r
  where
    f' = f . (V.toList ds ++)
    r = Map.foldrWithKey (\x tr xs -> foldr (f' . (:) x) xs tr) z e

foldMap :: (Monoid r) => ([c] -> r) -> TSet c -> r
foldMap f (TSet (Node ds a e))
  | a         = f' [] `mappend` r
  | otherwise = r
  where
    f' = f . (V.toList ds ++)
    r = Map.foldMapWithKey (\c subTrie ->
          foldMap (f' . (c :)) subTrie) e

foldl' :: (r -> [c] -> r) -> r -> TSet c -> r
foldl' f z = List.foldl' f z . enumerate

-- * Construction
empty :: TSet c
empty = TSet (Node V.empty False Map.empty)

-- | @epsilon = singleton []@
epsilon :: TSet c
epsilon = TSet (Node V.empty True Map.empty)

singleton :: [c] -> TSet c
singleton cs = TSet (Node (V.fromList cs) True Map.empty)

insert :: (Ord c, Foldable f) => f c -> TSet c -> TSet c
insert cs = union (singleton (F.toList cs))

delete :: (Ord c, Foldable f) => f c -> TSet c -> TSet c
delete cs t = difference t (singleton (F.toList cs))

-- * Combine
union :: (Ord c) => TSet c -> TSet c -> TSet c
union (TSet (Node cs1 a1 e1)) (TSet (Node cs2 a2 e2)) = loop 0
  where
    n1 = V.length cs1
    n2 = V.length cs2
    loopMax = min (V.length cs1) (V.length cs2)

    loop i
      | i < loopMax =
          let common = V.take i $ if n1 < n2 then cs1 else cs2
          in case ci1 == ci2 of
               True  -> loop (i+1)
               False -> TSet $ Node common False $ Map.fromList
                          [ (ci1, TSet (Node cs1' a1 e1))
                          , (ci2, TSet (Node cs2' a2 e2)) ]
      | n1 == n2 =
          let e' = Map.unionWith union e1 e2
              a' = a1 || a2
          in TSet $ Node cs1 a' e'
      | n1 < n2 =
          let e1' = Map.insertWith union ci2 (TSet (Node cs2' a2 e2)) e1
          in TSet $ Node cs1 a1 e1'
      | otherwise {- n1 > n2 -} =
          let e2' = Map.insertWith union ci1 (TSet (Node cs1' a1 e1)) e2
          in TSet $ Node cs2 a2 e2'
     where
       ci1 = cs1 V.! i
       ci2 = cs2 V.! i
       cs1' = V.drop (i+1) cs1
       cs2' = V.drop (i+1) cs2

intersection :: (Ord c) => TSet c -> TSet c -> TSet c
intersection x y = fromMaybe empty $ intersection_ x y

intersection_ :: (Ord c) => TSet c -> TSet c -> Maybe (TSet c)
intersection_ (TSet (Node cs1 a1 e1)) (TSet (Node cs2 a2 e2)) = loop 0
  where
    n1 = V.length cs1
    n2 = V.length cs2
    loopMax = min (V.length cs1) (V.length cs2)

    loop i
      | i < loopMax =
          case ci1 == ci2 of
             True  -> loop (i+1)
             False -> Nothing
      | n1 == n2 =
          let e' = Map.mapMaybe id $ Map.intersectionWith intersection_ e1 e2
              a' = a1 && a2
              t' = TSet $ Node cs1 a' e'
          in if null t' then Nothing else Just t'
      | n1 < n2 =
          let t2' = TSet $ Node cs2' a2 e2
              prefix = V.take (i+1) cs2
          in case Map.lookup ci2 e1 of
               Nothing -> Nothing
               Just t1' -> appendPrefix prefix <$> intersection_ t1' t2'
      | otherwise {- n1 > n2 -} =
          let t1' = TSet $ Node cs1' a1 e1
              prefix = V.take (i+1) cs1
          in case Map.lookup ci1 e2 of
               Nothing -> Nothing
               Just t2' -> appendPrefix prefix <$> intersection_ t1' t2'
     where
       ci1 = cs1 V.! i
       ci2 = cs2 V.! i
       cs1' = V.drop (i+1) cs1
       cs2' = V.drop (i+1) cs2

appendPrefix :: V.Vector c -> TSet c -> TSet c
appendPrefix prefix (TSet (Node cs a e)) =
  TSet (Node (prefix V.++ cs) a e)

difference :: (Ord c) => TSet c -> TSet c -> TSet c
difference x y = fromMaybe empty $ difference_ x y

difference_ :: (Ord c) => TSet c -> TSet c -> Maybe (TSet c)
difference_ (TSet (Node cs1 a1 e1)) t2 =
  case beginWith t2 (V.toList cs1) of
    TSet (Node cs2 a2 e2)
      | V.null cs2 ->
          let a' = a1 && not a2
              e' = Map.differenceWith difference_ e1 e2
              t' = TSet (Node cs1 a' e')
          in if null t' then Nothing else Just t'
      | otherwise ->
          let c2 = cs2 V.! 0
              t2' = TSet (Node (V.drop 1 cs2) a2 e2)
              e' = Map.update (`difference_` t2') c2 e1
              t' = TSet (Node cs1 a1 e')
          in if null t' then Nothing else Just t'

{-
append :: (Ord c) => TSet c -> TSet c -> TSet c
append = _

-- * Other operations

prefixes :: TSet c -> TSet c
prefixes = _

suffixes :: (Ord c) => TSet c -> TSet c
suffixes = _

infixes :: (Ord c) => TSet c -> TSet c
infixes = suffixes . prefixes
-}

-- * Conversion
toList, toAscList :: TSet c -> [[c]]
toList = enumerate
toAscList = enumerate

fromList :: (Ord c) => [[c]] -> TSet c
fromList = List.foldl' (flip insert) empty

fromAscList :: (Eq c) => [[c]] -> TSet c
fromAscList [] = empty
fromAscList [cs] = singleton cs
fromAscList xs = unfoldTSet step xs
  where step ys = case groupStrs ys of
          (a, es) -> (a, Map.fromDistinctAscList es)

groupStrs :: (Eq c) => [[c]] -> (Bool, [(c,[[c]])])
groupStrs = List.foldr pushStr (False, [])
  where
    pushStr [] (_, gs) = (True, gs)
    pushStr (c:cs) (hasNull, gs) =
      case gs of
        (d, dss):rest | c == d -> (hasNull, (d, cs:dss):rest)
        _                      -> (hasNull, (c, [cs]):gs)

unfoldTSet :: (s -> (Bool, Map c s)) -> s -> TSet c
unfoldTSet step = go
  where
    go s = case step' s of
      (ds, a, e) -> TSet $ Node (V.fromList ds) a (Map.map go e)

    step' s = case step s of
      (a, e) | not a && Map.size e == 1 ->
                 let (c,s') = head $ Map.toList e
                     (ds,a',e') = step' s'
                 in (c:ds,a',e')
             | otherwise                ->
                 ([], a, e)

toSet :: TSet c -> Set [c]
toSet = Set.fromDistinctAscList . enumerate

fromSet :: (Eq c) => Set [c] -> TSet c
fromSet = fromAscList . Set.toAscList

----------------------

foldTSet :: (Node c r -> r) -> TSet c -> r
foldTSet f = go
  where go (TSet node) = f (fmap go node)

paraTSet :: (Node c (TSet c, r) -> r) -> TSet c -> r
paraTSet f = go
  where go (TSet node) = f (fmap (id &&& go) node)
