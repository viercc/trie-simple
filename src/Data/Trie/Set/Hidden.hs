{-# LANGUAGE DeriveTraversable #-}
module Data.Trie.Set.Hidden(
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
  insert, insert_foldr, delete, delete_foldr,
  -- * Combine
  union, intersection, difference,
  append,
  -- * Other operations
  prefixes, suffixes, infixes,
  -- * Conversion
  fromList, toList,
  fromAscList, toAscList,
  fromSet, toSet,
  -- * Parsing
  toParser, toParser_,
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
import qualified Data.List       as List (foldr, foldl')
import           Data.Maybe      (fromMaybe)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Control.Arrow ((&&&))

import Control.DeepSeq

data Node c r = Node !Bool !(Map c r)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

instance (NFData c, NFData r) => NFData (Node c r) where
  rnf (Node a e) = rnf a `seq` rnf e

newtype TSet c = TSet { getNode :: Node c (TSet c) }
  deriving (Eq, Ord)

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
member [] (TSet (Node a _)) = a
member (c:cs) (TSet (Node _ e)) =
  case Map.lookup c e of
    Nothing -> False
    Just t' -> member cs t'

notMember :: (Ord c) => [c] -> TSet c -> Bool
notMember cs = not . member cs

-- | @beginWith t xs@ returns new TSet @t'@ which contains
--   all string @ys@ such that @t@ contains @xs ++ ys@.
beginWith :: (Ord c) => TSet c -> [c] -> TSet c
beginWith t       []               = t
beginWith (TSet (Node _ e)) (c:cs) = 
  case Map.lookup c e of
    Nothing -> empty
    Just t' -> beginWith t' cs

null :: TSet c -> Bool
null (TSet (Node a e)) = not a && Map.null e

-- | Returns number of elements. @count@ takes O(number of nodes)
--   unlike 'Set.size' which is O(1).
count :: TSet c -> Int
count = foldTSet count'
  where
    count' (Node a e) =
      (if a then 1 else 0) + sum e

-- | List of all elements.
enumerate :: TSet c -> [[c]]
enumerate = foldr (:) []

{-
from this post by u/foBrowsing:
  https://www.reddit.com/r/haskell/comments/8krv31/how_to_traverse_a_trie/dzaktkn/
-}
foldr :: ([c] -> r -> r) -> r -> TSet c -> r
foldr f z (TSet (Node a e))
  | a         = f [] r
  | otherwise = r
  where
    r = Map.foldrWithKey (\x tr xs -> foldr (f . (:) x) xs tr) z e

foldMap :: (Monoid r) => ([c] -> r) -> TSet c -> r
foldMap f (TSet (Node a e))
  | a         = f [] `mappend` r
  | otherwise = r
  where
    r = Map.foldMapWithKey (\c subTrie ->
          foldMap (f . (c :)) subTrie) e

foldl' :: (r -> [c] -> r) -> r -> TSet c -> r
foldl' f z = List.foldl' f z . enumerate

-- * Construction
empty :: TSet c
empty = TSet (Node False Map.empty)

-- | @epsilon = singleton []@
epsilon :: TSet c
epsilon = TSet (Node True Map.empty)

singleton :: [c] -> TSet c
singleton = List.foldr cons epsilon
  where
    cons c t = TSet (Node False (Map.singleton c t))

insert :: (Ord c) => [c] -> TSet c -> TSet c
insert [] (TSet (Node _ e)) = TSet (Node True e)
insert (c:cs) (TSet (Node a e)) =
  let e' = Map.insertWith (const (insert cs)) c (singleton cs) e
  in TSet (Node a e')
{-# INLINE insert #-}

insert_foldr :: (Ord c, Foldable f) => f c -> TSet c -> TSet c
insert_foldr = F.foldr f b
  where
    b (TSet (Node _ e)) = TSet (Node True e)
    f x xs (TSet (Node a e)) =
      let e' = Map.insertWith (const xs) x (xs empty) e
      in TSet (Node a e')
{-# INLINE insert_foldr #-}

delete :: (Ord c) => [c] -> TSet c -> TSet c
delete cs t = fromMaybe empty $ delete_ cs t
{-# INLINE delete #-}

delete_ :: (Ord c) => [c] -> TSet c -> Maybe (TSet c)
delete_ [] (TSet (Node _ e)) =
  if Map.null e then Nothing else Just (TSet (Node False e))
delete_ (c:cs) (TSet (Node a e)) =
  let e' = Map.update (delete_ cs) c e
      t' = TSet (Node a e')
  in if null t' then Nothing else Just t'
{-# INLINE delete_ #-}

delete_foldr :: (Ord c, Foldable f) => f c -> TSet c -> TSet c
delete_foldr cs t = fromMaybe empty $ delete_foldr_ cs t
{-# INLINE delete_foldr #-}

delete_foldr_ :: (Ord c, Foldable f) => f c -> TSet c -> Maybe (TSet c)
delete_foldr_ = F.foldr f b
  where
    b (TSet (Node _ e)) =
      if Map.null e then Nothing else Just (TSet (Node False e))     
    f x xs (TSet (Node a e)) =
      let e' = Map.update xs x e
          t' = TSet (Node a e')
      in if null t' then Nothing else Just t'
{-# INLINE delete_foldr_ #-}

-- * Combine
union :: (Ord c) => TSet c -> TSet c -> TSet c
union (TSet (Node ax ex)) (TSet (Node ay ey)) = TSet (Node az ez)
  where
    az = ax || ay
    ez = Map.unionWith union ex ey

intersection :: (Ord c) => TSet c -> TSet c -> TSet c
intersection x y = fromMaybe empty $ intersection_ x y

intersection_ :: (Ord c) => TSet c -> TSet c -> Maybe (TSet c)
intersection_ (TSet (Node ax ex)) (TSet (Node ay ey)) =
    if not az && Map.null ez
      then Nothing
      else Just $ TSet (Node az ez)
  where
    az = ax && ay
    emz = Map.intersectionWith intersection_ ex ey
    ez = Map.mapMaybe id emz

difference :: (Ord c) => TSet c -> TSet c -> TSet c
difference x y = fromMaybe empty $ difference_ x y

difference_ :: (Ord c) => TSet c -> TSet c -> Maybe (TSet c)
difference_ (TSet (Node ax ex)) (TSet (Node ay ey)) =
    if not az && Map.null ez
      then Nothing
      else Just $ TSet (Node az ez)
  where
    az = ax > ay
    ez = Map.differenceWith difference_ ex ey

append :: (Ord c) => TSet c -> TSet c -> TSet c
append _ y | null y = empty
append (TSet (Node ax ex)) y = f (TSet (Node False ez))
  where
    ez = Map.map (`append` y) ex
    f = if ax then union y else id

-- * Other operations

prefixes :: TSet c -> TSet c
prefixes t | null t    = empty
           | otherwise = foldTSet prefixes' t
  where
    prefixes' (Node _ e) = TSet (Node True e)

suffixes :: (Ord c) => TSet c -> TSet c
suffixes = paraTSet suffixes'
  where
    suffixes' nx = union (TSet (fst <$> nx)) (F.foldMap snd nx)

infixes :: (Ord c) => TSet c -> TSet c
infixes = suffixes . prefixes

-- * Conversion
toList, toAscList :: TSet c -> [[c]]
toList = enumerate
toAscList = enumerate

fromList :: (Ord c) => [[c]] -> TSet c
fromList = List.foldl' (flip insert) empty

fromAscList :: (Eq c) => [[c]] -> TSet c
fromAscList [] = empty
fromAscList [cs] = singleton cs
fromAscList xs =
  let (a,es) = groupStrs xs
      e' = Map.fromDistinctAscList $ map (fmap fromAscList) es
  in TSet (Node a e')

groupStrs :: (Eq c) => [[c]] -> (Bool, [(c,[[c]])])
groupStrs = List.foldr pushStr (False, [])
  where
    pushStr [] (_, gs) = (True, gs)
    pushStr (c:cs) (hasNull, gs) =
      case gs of
        (d, dss):rest | c == d -> (hasNull, (d, cs:dss):rest)
        _                      -> (hasNull, (c, [cs]):gs)

toSet :: TSet c -> Set [c]
toSet = Set.fromDistinctAscList . enumerate

fromSet :: (Eq c) => Set [c] -> TSet c
fromSet = fromAscList . Set.toAscList

-- * Parsing

-- | Construct a \"parser\" which recognizes member strings
--   of a TSet.
--
--   * @char@ constructs a parser which recognizes a character.
--   * @eot@ recognizes the end of a token.
toParser :: (Alternative f) =>
  (c -> f a) -- ^ char
  -> f b     -- ^ eot
  -> TSet c -> f [a]
toParser char eot = foldTSet enumerateA'
  where
    enumerateA' (Node a e) =
      (if a then [] <$ eot else Ap.empty) <|>
      F.asum [ (:) <$> char c <*> as | (c, as) <- Map.toAscList e ]

-- | Construct a \"parser\" which recognizes member strings
--   of a TSet.
--   It discards the information which string it is recognizing.
--
--   * @char@ constructs a parser which recognizes a character.
--   * @eot@ recognizes the end of a token.
toParser_ :: (Alternative f) =>
  (c -> f a) -- ^ char
  -> f b     -- ^ eot
  -> TSet c -> f ()
toParser_ char eot = foldTSet enumerateA'
  where
    enumerateA' (Node a e) =
      (if a then () <$ eot else Ap.empty) <|>
      F.asum [ char c *> as | (c, as) <- Map.toAscList e ]

----------------------

foldTSet :: (Node c r -> r) -> TSet c -> r
foldTSet f = go
  where go (TSet node) = f (fmap go node)

paraTSet :: (Node c (TSet c, r) -> r) -> TSet c -> r
paraTSet f = go
  where go (TSet node) = f (fmap (id &&& go) node)
