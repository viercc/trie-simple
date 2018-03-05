{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Data.ADFA.Algorithm(
  -- * Query
  enumerate, stringCount, isEmpty, equivalent,
  foldNodes, foldNodes',
  -- * Build
  string, strings,
  -- * Combine
  union, intersection, difference, append,
  -- * Other operations
  prefixes, suffixes, infixes,
  -- * Cleaning ADFA without changing its semantics
  topSort, prune, minify,
  -- * Conversion
  fromList, toList,
  fromAscList, toAscList,
  fromSet, toSet
) where

import           Data.Tuple          (swap)
import           Data.Maybe          (catMaybes)
import           Data.Bifunctor
import qualified Data.Map.Lazy       as LMap
import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as Map
import           Data.Set            (Set)
import qualified Data.Set            as Set
import qualified Data.Vector         as V

import qualified Control.Applicative
import           Control.Monad.State

import           Data.ADFA.Internal
import           Util

foldNodes :: (Bool -> Map c r -> r) -> ADFA c -> r
foldNodes f dfa = go (rootNode dfa)
  where
    go x =
      let Node acceptsX edgesX = dfa ! x
      in f acceptsX (LMap.map go edgesX)

-- | Basically same to @foldNodes@ but perform memoized recursion
--   instead of normal recursion.
foldNodes' :: (Bool -> Map c r -> r) -> ADFA c -> r
foldNodes' f dfa = foldNodesTableWithKey (const f) dfa V.! getNodeId (rootNode dfa)

-- | Enumerates all unique strings an ADFA accepts.
--
-- > strings . enumerate = id
enumerate :: ADFA c -> [[c]]
enumerate = foldNodes f
  where
    f acc nexts =
      let nil = [ [] | acc ]
          advance = [ c:cs | (c, css) <- Map.toAscList nexts, cs <- css ]
      in nil ++ advance

isEmpty :: ADFA c -> Bool
isEmpty = foldNodes f
  where
    f acc nexts = not acc && and nexts

-- | Counts the number of all unique strings an ADFA accepts.
stringCount :: ADFA c -> Int
stringCount = foldNodes' f
  where
    f acc nexts = (if acc then 1 else 0) + sum nexts

-- | Equivalence
equivalent :: (Ord c) => ADFA c -> ADFA c -> Bool
equivalent dfaA dfaB = eqv (rootNode dfaA', rootNode dfaB')
  where
    dfaA' = prune dfaA
    dfaB' = prune dfaB

    eqv (a,b) =
      let Node acceptsA edgesA = dfaA' ! a
          Node acceptsB edgesB = dfaB' ! b
          (onlyA, both, onlyB) = trisect edgesA edgesB
      in acceptsA == acceptsB &&
         Map.null onlyA &&
         Map.null onlyB &&
         all eqv both

-----------------------------------------------------

union :: (Ord c) => ADFA c -> ADFA c -> ADFA c
union dfaA dfaB = instantiate rootKey stepKey
  where
    rootKey = (Just (rootNode dfaA), Just (rootNode dfaB))
    
    node0 = Node False Map.empty

    stepKey (mayA, mayB) =
      let Node acceptsA edgesA = maybe node0 (dfaA !) mayA
          Node acceptsB edgesB = maybe node0 (dfaB !) mayB
      in Node (acceptsA || acceptsB) (merge edgesA edgesB)
    
    merge :: (Ord k) => Map k a -> Map k b -> Map k (Maybe a, Maybe b)
    merge as bs =
      let f (ma, _) (_, mb) = (ma, mb)
          as' = (\a -> (Just a, Nothing)) <$> as
          bs' = (\b -> (Nothing, Just b)) <$> bs
      in Map.unionWith f as' bs'

-- | Constructs an ADFA which accepts a string iff both ADFAs accept it.
intersection :: Ord c => ADFA c -> ADFA c -> ADFA c
intersection dfaA dfaB = instantiate rootKey stepKey
  where
    rootKey = (rootNode dfaA, rootNode dfaB)
    stepKey (a, b) =
      let Node acceptsA edgesA = dfaA ! a
          Node acceptsB edgesB = dfaB ! b
      in Node (acceptsA && acceptsB) (Map.intersectionWith (,) edgesA edgesB)

difference :: (Ord c) => ADFA c -> ADFA c -> ADFA c
difference dfaA dfaB = instantiate rootKey stepKey
  where
    rootKey = (rootNode dfaA, Just (rootNode dfaB))
    
    stepKey (a, Just b) =
      let Node acceptsA edgesA = dfaA ! a
          acceptsKey = acceptsA && not (dfaB `accepts` b)
          edges = Map.mapWithKey (\c a' -> (a', step dfaB c b)) edgesA
      in Node acceptsKey edges
    stepKey (a, Nothing) =
      let Node acceptsA edgesA = dfaA ! a
          edges = Map.map (\a' -> (a', Nothing)) edgesA
      in Node acceptsA edges

append :: (Ord c) => ADFA c -> ADFA c -> ADFA c
append dfaA dfaB = instantiate rootKey stepKey
  where
    rootKey = (Just (rootNode dfaA), Set.empty)
    
    node0 = Node False Map.empty
    
    (<|>) = (Control.Applicative.<|>)
    unionKey (mayA1, bs1) (mayA2, bs2) = (mayA1 <|> mayA2, bs1 `Set.union` bs2)
    fromA a = (Just a, Set.empty)
    fromB b = (Nothing, Set.singleton b)
    
    rootNodeB = dfaB ! rootNode dfaB
    
    stepKey (mayA, bSet) =
      let Node acceptsA edgesA = maybe node0 (dfaA !) mayA
          nodeBs = (dfaB !) <$> Set.toList bSet
          nodeBs' = if acceptsA then rootNodeB : nodeBs else nodeBs
          acceptsKey = any isAccepted nodeBs'
          edgesKey =
            Map.unionsWith unionKey $
              Map.map fromA edgesA :
              map (Map.map fromB . outEdges) nodeBs'
      in Node acceptsKey edgesKey

-- | Accept all prefixes of currently accepted strings.
prefixes :: ADFA c -> ADFA c
prefixes dfa@(MkDFA _ root) = MkDFA nodes' root
  where
    nodes' = snd <$> foldNodesTableWithKey prefixes' dfa
    prefixes' x acceptsX edgesX =
      let edgesX' = fst <$> edgesX
          nonEmptyX = acceptsX || any (isAccepted . snd) edgesX
      in (x, Node nonEmptyX edgesX')

-- | Accepts all suffixes of currently accepted strings.
suffixes :: (Ord c) => ADFA c -> ADFA c
suffixes dfa = instantiate rootKey stepKey
  where
    reachable accum [] = accum
    reachable accum (x:xs)
      | x `Set.member` accum = reachable accum xs
      | otherwise            =
          let accum' = Set.insert x accum
              ys = Map.elems $ dfa !> x
          in reachable accum' (ys ++ xs)
    
    rootKey = reachable Set.empty [rootNode dfa]
    stepKey xs = 
      let node0 = Node False Map.empty
          merge (Node acceptsKey edgesKey) x =
            let Node acceptsX edgesX = dfa ! x
                acceptsKey' = acceptsKey || acceptsX
                edgesX' = Map.map Set.singleton edgesX
                edgesKey' = Map.unionWith Set.union edgesKey edgesX'
            in Node acceptsKey' edgesKey'
      in Set.foldl' merge node0 xs

-- | Accepts all contiguous substrings of currently accepted strings.
infixes :: (Ord c) => ADFA c -> ADFA c
infixes = suffixes . prefixes

-- | Relabel nodes to make all paths have increasing order.
--   Applying @topSort@ also eliminates unreachable nodes,
--   so it makes 'garbageCollect' unnecessary.
topSort :: ADFA c -> ADFA c
topSort (MkDFA nodes root) =
  renumber root . map (second tupleToNode) $ topologicalSort root idx (Map.elems . snd)
  where
    idx (NodeId x) = case nodes V.! x of
      Node accX edgesX -> (accX, edgesX)

-- Remove empty nodes which is not accept node and only goes to other empty nodes.
prune :: ADFA c -> ADFA c
prune dfa = if rootIsEmpty then empty else dfa'
  where
    table = foldNodesTableWithKey pruneStep dfa
    root = rootNode dfa
    rootIsEmpty = case table V.! getNodeId root of
      Nothing -> True
      Just _ -> False
    dfa' = renumber root . catMaybes $ V.toList table
    
    pruneStep :: NodeId -> Bool -> Map c (Maybe (NodeId, Node c NodeId)) -> Maybe (NodeId, Node c NodeId)
    pruneStep x acceptsX nexts =
      let edges = Map.mapMaybe (fmap fst) nexts
          isEmptyX = not acceptsX && Map.null edges
      in if isEmptyX then Nothing else Just (x, Node acceptsX edges)

type ReverseIndex c = Map (Node c NodeId) NodeId

-- | Minimizes an ADFA by removing redundant nodes.
--   Applying @minify@ also removes unreachable nodes,
--   so it makes 'garbageCollect' and 'prune' unnecessary.
minify :: forall c. (Ord c) => ADFA c -> ADFA c
minify dfa =
  postprocess $ execState (go root) (dup0, subst0)
  where
    root :: NodeId
    root = rootNode dfa
    
    subst0 :: Map NodeId (Maybe NodeId)
    subst0 = Map.empty
    
    dup0 :: ReverseIndex c
    dup0 = Map.empty
    
    addNode :: NodeId -> Node c NodeId -> ReverseIndex c ->
                (NodeId, ReverseIndex c)
    addNode x node dup =
      case Map.insertLookupWithKey (\_ _ x0 -> x0) node x dup of
        (Nothing, dup') -> (x, dup')
        (Just x0, dup') -> (x0, dup')
    
    go :: NodeId -> State (ReverseIndex c, Map NodeId (Maybe NodeId)) (Maybe NodeId)
    go x = do
      (_, subst) <- get
      case Map.lookup x subst of
        Nothing -> do
          let Node accepted neighbours = dfa ! x
          neighbours' <- traverse go neighbours
          let neighbours'' = Map.mapMaybe id neighbours'
              getRemoved = Map.null neighbours''  && not accepted
          if getRemoved
            then do
              modifySnd (Map.insert x Nothing)
              return Nothing
            else do
              x0 <- stateFst (addNode x (Node accepted neighbours''))
              modifySnd (Map.insert x (Just x0))
              return (Just x0)
        Just r -> return r
    
    postprocess :: (ReverseIndex c, Map NodeId (Maybe NodeId)) -> ADFA c
    postprocess (dup, subst) =
      case Map.lookup root subst of
        Just (Just root') ->
          let table = map swap $ Map.toList dup
          in  renumber root' table
        _ -> empty

-- * Conversion

toList, toAscList :: ADFA c -> [[c]]
toList = enumerate
toAscList = enumerate

fromList :: (Ord c) => [[c]] -> ADFA c
fromList = strings

fromAscList :: (Eq c) => [[c]] -> ADFA c
fromAscList css = treeInstantiate css down
  where down xss = case groupStrs xss of
          (t, gs) -> Node t (Map.fromAscList gs)

toSet :: ADFA c -> Set [c]
toSet = Set.fromDistinctAscList . toAscList

fromSet :: Eq c => Set [c] -> ADFA c
fromSet = fromAscList . Set.toAscList

-- Utilities

foldNodesTableWithKey :: (NodeId -> Bool -> Map c r -> r) -> ADFA c -> V.Vector r
foldNodesTableWithKey f dfa = table
  where
    g x (Node t e) = f (NodeId x) t (LMap.map ((table V.!) . getNodeId) e)
    table = V.imap g (getNodes dfa)

modifySnd :: (MonadState (a,b) m) => (b -> b) -> m ()
modifySnd f = get >>= \(a, b) -> let !b' = f b in put (a, b')

stateFst :: (MonadState (a,b) m) => (a -> (x,a)) -> m x
stateFst f = get >>= \(a, b) -> let (x, !a') = f a in put (a', b) >> return x

tupleToNode :: (Bool, Map c k) -> Node c k
tupleToNode (t, e) = Node t e
