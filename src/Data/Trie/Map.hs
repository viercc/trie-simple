{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Trie.Map(
  -- * Types
  TMap(..),
  -- * Queries
  match,
  lookup,
  member, notMember,
  null, count,
  keys, elems,
  -- * Construction
  empty, just,
  singleton,
  
  -- * Single item modification
  insertWith, insert,
  deleteWith, delete,

  adjust, revise, update, alter,

  -- * Combine
  union, unionWith,
  intersection, intersectionWith,
  difference, differenceWith,
  appendWith,
  
  -- * Conversion
  toList, fromList,
  toAscList, fromAscList,
  toMap, fromMap,
  keysTSet, fromTSet,

  -- * Parsing
  toParser, toParser_, toParser__,

  -- * Traversing with keys
  traverseWithKey, mapWithKey, foldMapWithKey,
  
  -- * Internals
  Node(..),
  foldTMap,
)
where

import Prelude hiding (lookup, null)

import           Data.Functor.Identity
import           Data.Functor.Const

import           Data.Semigroup

import           Control.Applicative hiding (empty)
import qualified Control.Applicative as Ap(empty)

import           Control.Monad

import qualified Data.Foldable    as F
import           Data.Traversable
import qualified Data.List        as List (foldl')
import           Data.Maybe       (fromMaybe, isJust, isNothing)
import           Data.Map.Lazy    (Map)
import qualified Data.Map.Lazy    as Map

import qualified Data.Trie.Set    as TSet
import           Data.Trie.Set    (TSet(..))

import Control.DeepSeq

data Node c a r = Node !(Maybe a) !(Map c r)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

instance (NFData c, NFData a, NFData r) => NFData (Node c a r) where
  rnf (Node a e) = rnf a `seq` rnf e

newtype TMap c a = TMap { getNode :: Node c a (TMap c a) }
  deriving (Eq, Ord)

instance (Show c, Show a) => Show (TMap c a) where
  showsPrec p t = showParen (p > 10) $ showString "fromList " . showsPrec 11 (toList t)

instance (NFData c, NFData a) => NFData (TMap c a) where
  rnf (TMap node) = rnf node

-- * Queries
match :: (Ord c) => [c] -> TMap c a -> (Maybe a, TMap c a)
match []     t@(TMap (Node ma _)) = (ma, t)
match (c:cs)   (TMap (Node _  e)) =
  case Map.lookup c e of
    Nothing -> (Nothing, empty)
    Just t' -> match cs t'

lookup :: (Ord c) => [c] -> TMap c a -> Maybe a
lookup cs = fst . match cs

member, notMember :: (Ord c) => [c] -> TMap c a -> Bool
member cs = isJust . lookup cs
notMember cs = isNothing . lookup cs

null :: TMap c a -> Bool
null (TMap (Node ma e)) = isNothing ma && Map.null e
{- We ensure all @TMap@ values exposed to users have no
   redundant node. -}

count :: TMap c a -> Int
count = F.length

keys :: TMap c a -> [[c]]
keys = foldTMap keys'
  where
    keys' (Node ma e) =
      [ [] | isJust ma ] ++
      [ c:cs' | (c,css') <- Map.toList e, cs' <- css' ]

elems :: TMap c a -> [a]
elems = F.toList

-- * Construction
empty :: TMap c a
empty = TMap (Node Nothing Map.empty)

just :: a -> TMap c a
just a = TMap (Node (Just a) Map.empty)

singleton :: [c] -> a -> TMap c a
singleton cs a0 = foldr cons (just a0) cs
  where
    cons c t = TMap (Node Nothing (Map.singleton c t))

-- * Single-item modification
insertWith :: (Ord c) => (a -> a -> a) -> [c] -> a -> TMap c a -> TMap c a
insertWith f cs a = revise f' cs 
  where
    f' = maybe a (f a)

insert :: (Ord c) => [c] -> a -> TMap c a -> TMap c a
insert = insertWith const

deleteWith :: (Ord c) => (b -> a -> Maybe a) -> [c] -> b -> TMap c a -> TMap c a
deleteWith f cs b = update (f b) cs

delete :: (Ord c) => [c] -> TMap c a -> TMap c a
delete = update (const Nothing)

adjust :: (Ord c) => (a -> a) -> [c] -> TMap c a -> TMap c a
adjust f = go
  where
    go []     (TMap (Node ma e)) = TMap (Node (f <$> ma) e)
    go (c:cs) (TMap (Node ma e)) =
      let e' = Map.adjust (go cs) c e
      in TMap (Node ma e')

revise :: (Ord c) => (Maybe a -> a) -> [c] -> TMap c a -> TMap c a
revise f = go
  where
    go []     (TMap (Node ma e)) = TMap (Node (Just (f ma)) e)
    go (c:cs) (TMap (Node ma e)) =
      let tNew = singleton cs (f Nothing)
          e' = Map.insertWith (const (go cs)) c tNew e
      in TMap (Node ma e')

update :: (Ord c) => (a -> Maybe a) -> [c] -> TMap c a -> TMap c a
update f cs = fromMaybe empty . update_ f cs

update_ :: (Ord c) => (a -> Maybe a) -> [c] -> TMap c a -> Maybe (TMap c a)
update_ f = go
  where
    go [] (TMap (Node ma e)) =
      let ma' = ma >>= f
      in if isNothing ma' && Map.null e
           then Nothing
           else Just $ TMap (Node ma' e)
    go (c:cs) (TMap (Node ma e)) =
      let e' = Map.update (go cs) c e
      in if isNothing ma && Map.null e'
           then Nothing
           else Just $ TMap (Node ma e')

alter :: (Ord c) => (Maybe a -> Maybe a) -> [c] -> TMap c a -> TMap c a
alter f cs = fromMaybe empty . alter_ f cs

alter_ :: (Ord c) => (Maybe a -> Maybe a) -> [c] -> TMap c a -> Maybe (TMap c a)
alter_ f = go
  where
    go [] (TMap (Node ma e)) =
      let ma' = f ma
      in if isNothing ma' && Map.null e
           then Nothing
           else Just $ TMap (Node ma' e)
    go (c:cs) (TMap (Node ma e)) =
      let e' = Map.alter (aux cs) c e
      in if isNothing ma && Map.null e'
           then Nothing
           else Just $ TMap (Node ma e')
    
    aux cs Nothing  = singleton cs <$> f Nothing
    aux cs (Just t) = go cs t

-- * Combine
union :: (Ord c) => TMap c a -> TMap c a -> TMap c a
union = unionWith const

unionWith :: (Ord c) => (a -> a -> a) -> TMap c a -> TMap c a -> TMap c a
unionWith f = go
  where
    go (TMap (Node mat et)) (TMap (Node mau eu)) =
      let maz = case (mat, mau) of
            (Nothing, Nothing) -> Nothing
            (Just at, Nothing) -> Just at
            (Nothing, Just au) -> Just au
            (Just at, Just au) -> Just (f at au)
          ez = Map.unionWith go et eu
      in TMap (Node maz ez)

intersection :: (Ord c) => TMap c a -> TMap c b -> TMap c a
intersection = intersectionWith (\a _ -> Just a)

intersectionWith :: (Ord c) => (a -> b -> Maybe r) -> TMap c a -> TMap c b -> TMap c r
intersectionWith f x y = fromMaybe empty $ go x y
  where
    go (TMap (Node ma ex)) (TMap (Node mb ey)) =
      if isNothing mr && Map.null ez
        then Nothing
        else Just $ TMap (Node mr ez)
      where
        mr = do a <- ma
                b <- mb
                f a b
        emz = Map.intersectionWith go ex ey
        ez = Map.mapMaybe id emz

difference :: (Ord c) => TMap c a -> TMap c b -> TMap c a
difference = differenceWith (\_ _ -> Nothing)

differenceWith :: (Ord c) => (a -> b -> Maybe a) -> TMap c a -> TMap c b -> TMap c a
differenceWith f x y = fromMaybe empty $ go x y
  where
    go (TMap (Node ma ex)) (TMap (Node mb ey)) =
      if isNothing mr && Map.null ez
        then Nothing
        else Just $ TMap (Node mr ez)
      where
        mr = case (ma, mb) of
          (Nothing, _)       -> Nothing
          (Just a,  Nothing) -> Just a
          (Just a,  Just b)  -> f a b
        ez = Map.differenceWith go ex ey

{- |
Make new @TMap@ from two @TMap@s. Constructed @TMap@
has keys which are concatenation of any combination from
two input maps.

Corresponding values for these keys are combined with given function
of type @(x -> y -> z)@. If two different concatenations yield
a same key, Corresponding values for these keys are combined with
a 'Semigroup' operation @<>@.

There is no guarantees on which order corresponding duplicate
keys are combined. So it must be commutative semigroup to get stable result.

Example
=======

> let x = fromList [("a", Sum 1), ("aa", Sum 2)]
>     y = fromList [("aa", Sum 10), ("aaa", Sum 20)]
> 
> appendWith (*) x y =
>   fromList [ ("aaa", Sum $ 1 * 10)
>            , ("aaaa", Sum $ 1 * 20 + 2 * 10)
>            , ("aaaaa", Sum $ 2 * 20) ]

-}
appendWith :: (Ord c, Semigroup z) => (x -> y -> z) ->
  TMap c x -> TMap c y -> TMap c z
appendWith f x y =
  if null y
    then empty
    else go x
  where
    go (TMap (Node Nothing e)) =
      let e' = Map.map go e
      in TMap (Node Nothing e')
    go (TMap (Node (Just ax) e)) =
      let TMap (Node maz e') = fmap (f ax) y
          e'' = Map.map go e
          e''' = Map.unionWith (unionWith (<>)) e' e''
      in TMap (Node maz e''')

-- * Instances

instance Functor (TMap c) where
  fmap = fmapDefault

instance Foldable (TMap c) where
  foldMap = foldMapDefault

instance Traversable (TMap c) where
  traverse f = go
    where
      go (TMap (Node a e)) = TMap <$> (Node <$> traverse f a <*> traverse go e)

-- | 'unionWith'-based
instance (Ord c, Semigroup a) => Semigroup (TMap c a) where
  (<>) = unionWith (<>)
  stimes n = fmap (stimes n)

-- | 'unionWith'-based
instance (Ord c, Semigroup a) => Monoid (TMap c a) where
  mempty = empty
  mappend = (<>)

-- * Conversion

toList :: TMap c a -> [([c], a)]
toList = foldTMap toList'
  where
    toList' (Node ma e) =
      [ ([], a) | a <- F.toList ma ] ++
      [ (c:cs, a) | (c,pairs') <- Map.toAscList e, (cs,a) <- pairs' ]

fromList :: Ord c => [([c], a)] -> TMap c a
fromList = List.foldl' (flip (uncurry insert)) empty

toAscList :: TMap c a -> [([c], a)]
toAscList = toList

fromAscList :: Eq c => [([c], a)] -> TMap c a
fromAscList [] = empty
fromAscList [(cs, a)] = singleton cs a
fromAscList pairs = 
  let (ma, gs) = group_ pairs
      e = Map.fromDistinctAscList $ map (fmap fromAscList) gs
  in TMap (Node ma e)

group_ :: Eq c => [([c], a)] -> (Maybe a, [ (c, [ ([c], a) ]) ] )
group_ = foldr step (Nothing, [])
  where
    step ([], a) (ma, gs) = (ma <|> Just a, gs)
    step (c:cs, a) (ma, gs) = case gs of
      (d,ps'):rest | c == d  -> (ma, (d, (cs,a):ps'):rest)
      _ -> (ma, (c, [(cs,a)]):gs)

toMap :: TMap c a -> Map [c] a
toMap = Map.fromDistinctAscList . toAscList

fromMap :: (Eq c) => Map [c] a -> TMap c a
fromMap = fromAscList . Map.toAscList

keysTSet :: TMap c a -> TSet c
keysTSet = foldTMap keysTSet'
  where
    keysTSet' (Node ma e) =
      TSet (TSet.Node (isJust ma) e)

fromTSet :: ([c] -> a) -> TSet c -> TMap c a
fromTSet f = go []
  where
    go q (TSet (TSet.Node a e)) =
      let e' = Map.mapWithKey (\c -> go (c:q)) e
          a' = if a then Just (f (reverse q)) else Nothing
      in TMap (Node a' e')

-- * Parsing

toParser :: Alternative f => (c -> f c') -> TMap c a -> f ([c'], a)
toParser f = foldTMap toParser'
  where
    toParser' (Node ma e) =
      maybe Ap.empty (\a -> pure ([], a)) ma <|>
      F.asum [ consFst <$> f c <*> p' | (c, p') <- Map.toAscList e ]
    
    consFst c (cs, a) = (c:cs, a)

toParser_ :: Alternative f => (c -> f ()) -> TMap c a -> f a
toParser_ f = foldTMap toParser'
  where
    toParser' (Node ma e) =
      maybe Ap.empty pure ma <|>
      F.asum [ f c *> p' | (c, p') <- Map.toAscList e ]

toParser__ :: Alternative f => (c -> f ()) -> TMap c a -> f ()
toParser__ f = void . toParser_ f

-- * Traversing with keys

-- | Same semantics to following defintion, but have
--   more efficient implementation.
--
-- > traverseWithKey f = fmap fromAscList .
-- >                     traverse (\(cs,a) -> (,) cs <$> f cs a) .
-- >                     toAscList
traverseWithKey :: (Applicative f) =>
  ([c] -> a -> f b) -> TMap c a -> f (TMap c b)
traverseWithKey f = go []
  where
    go q (TMap (Node ma e)) =
      let step c = go (c : q)
          e' = Map.traverseWithKey step e
          mb = maybe (pure Nothing)
                     (\a -> Just <$> f (reverse q) a)
                     ma
      in TMap <$> (Node <$> mb <*> e')

-- | Same semantics to following defintion, but have
--   more efficient implementation.
--
-- > traverseWithKey f = fmap fromAscList .
-- >                     map (\(cs,a) -> (cs,  f cs a)) .
-- >                     toAscList
mapWithKey :: ([c] -> a -> b) -> TMap c a -> TMap c b
mapWithKey f = runIdentity . traverseWithKey (\k a -> Identity (f k a))

foldMapWithKey :: (Monoid r) => ([c] -> a -> r) -> TMap c a -> r
foldMapWithKey f = getConst . traverseWithKey (\k a -> Const (f k a))

-- * Other operations

foldTMap :: (Node c a r -> r) -> TMap c a -> r
foldTMap f = go
  where go (TMap node) = f (fmap go node)
