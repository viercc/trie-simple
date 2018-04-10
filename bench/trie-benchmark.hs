module Main(main) where

import Criterion.Main

import qualified Data.Trie.Set as TSet
import Data.Trie.Map (TMap)
import qualified Data.Trie.Map as TMap

import Data.Monoid
import Data.List (sort, inits, tails, foldl')
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import qualified Data.Vector as V
import Data.Word

import qualified System.Random.MWC                as R
import qualified System.Random.MWC.CondensedTable as R
import qualified System.Random.MWC.Distributions  as R

numRandomStr :: Int
numRandomStr = 1000

seed :: Word32 -> V.Vector Word32
seed w = V.fromList [1573289798, 32614861, w]

dictAmEn, dictBrEn, dictAmEnShuffled, randomStrs :: IO [String]
dictAmEn = lines <$> readFile "/usr/share/dict/american-english"
dictBrEn = lines <$> readFile "/usr/share/dict/british-english"
dictAmEnShuffled =
  do g <- R.initialize (seed 1)
     ws <- V.fromList <$> dictAmEn
     V.toList <$> R.uniformShuffle ws g
randomStrs =
  do g <- R.initialize (seed 3)
     revReplicateM numRandomStr $ do
       n <- R.genFromTable distN g
       revReplicateM (n+1) (uniformAlphabet g)
  where
    distN = R.tableBinomial 12 0.33
    alphabet = V.fromList ['a' .. 'z']
    numAlphabet = V.length alphabet
    uniformAlphabet g = (alphabet V.!) <$> R.uniformR (0, numAlphabet-1) g

main :: IO ()
main = defaultMain [ benchTSet, benchSet, benchTMap, benchMap ]

benchTSet :: Benchmark
benchTSet = bgroup "TSet" 
  [ bgroup "construction"
      [ env dictAmEnShuffled $ \dict -> bench "fromList" $ nf TSet.fromList dict
      , env (sort <$> dictAmEn) $ \sortedDict ->
          bench "fromAscList" $ nf TSet.fromAscList sortedDict ]
  , env (TSet.fromList <$> dictAmEn) $ \dict ->
      bgroup "query"
        [ bench "isEmpty" (nf TSet.null dict)
        , bench "stringCount" (nf TSet.count dict)
        , bench "enumerate10" (nf (take 10 . TSet.enumerate) dict)
        , env randomStrs $ \qs ->
            bench "match" (nf (\dict' -> map (`TSet.member` dict') qs) dict) ]
  , env (TSet.fromList <$> dictAmEn) $ \dictA ->
    env (TSet.fromList <$> dictBrEn) $ \dictB ->
    env (TSet.fromList <$> randomStrs) $ \dictSmall ->
      bgroup "combine"
        [ bench "union" (nf (uncurry TSet.union) (dictA, dictB))
        , bench "intersection" (nf (uncurry TSet.intersection) (dictA, dictB))
        , bench "difference" (nf (uncurry TSet.difference) (dictA, dictB))
        , bench "append" (nf (uncurry TSet.append) (dictSmall, dictSmall))
        , bench "prefixes" (nf TSet.prefixes dictA)
        , bench "suffixes" (nf TSet.suffixes dictB) ]
  ]

benchSet :: Benchmark
benchSet = bgroup "Set" 
  [ bgroup "construction"
      -- Set.fromList detects whether the input list is sorted
      -- and switch the algorithm based on it.
      -- Using shuffled dictionary avoids this optimization fires
      -- in this benchmark.
      [ env dictAmEnShuffled $ \dict -> bench "fromList" $ nf Set.fromList dict
      , env (sort <$> dictAmEn) $ \sortedDict ->
          bench "fromAscList" $ nf Set.fromAscList sortedDict ]
  , env (Set.fromList <$> dictAmEn) $ \dictSet ->
      bgroup "query"
        [ bench "isEmpty" (nf Set.null dictSet)
        , bench "stringCount" (nf Set.size dictSet)
        , bench "enumerate10" (nf (take 10 . Set.toList) dictSet)
        , env randomStrs $ \qs ->
            bench "match" (nf (\dictSet' -> map (`Set.member` dictSet') qs) dictSet) ]
  , env (Set.fromList <$> dictAmEn) $ \dictA ->
    env (Set.fromList <$> dictBrEn) $ \dictB ->
    env (Set.fromList <$> randomStrs) $ \dictSmall ->
      bgroup "combine"
        [ bench "union" (nf (uncurry Set.union) (dictA, dictB))
        , bench "intersection" (nf (uncurry Set.intersection) (dictA, dictB))
        , bench "difference" (nf (uncurry Set.difference) (dictA, dictB))
        , bench "append" (nf (uncurry setAppend) (dictSmall, dictSmall))
        , bench "prefixes" (nf setPrefixes dictA)
        , bench "suffixes" (nf setSuffixes dictB) ]
  ]

setAppend :: (Ord c) => Set [c] -> Set [c] -> Set [c]
setAppend ass bss = Set.unions
  [ Set.mapMonotonic (as ++) bss
      | as <- Set.toAscList ass ]

setPrefixes :: (Ord c) => Set [c] -> Set [c]
setPrefixes ass = Set.unions
  [ Set.fromDistinctAscList (inits as) | as <- Set.toAscList ass ]

setSuffixes :: (Ord c) => Set [c] -> Set [c]
setSuffixes ass = Set.fromList
  [ bs | as <- Set.toAscList ass, bs <- tails as ]


benchTMap :: Benchmark
benchTMap = bgroup "TMap" 
  [ bgroup "construction"
      [ env dictAmEnShuffled $ \dict ->
          bench "fromList" $ nf TMap.fromList [(w, length w) | w <- dict]
      , env (sort <$> dictAmEn) $ \sortedDict ->
          bench "fromAscList" $ nf TMap.fromAscList [(w, length w) | w <- sortedDict]
      ]
  , env (lenTMap <$> dictAmEn) $ \dict ->
      bgroup "query"
        [ bench "isEmpty" (nf TMap.null dict)
        , bench "stringCount" (nf TMap.count dict)
        , bench "enumerate10" (nf (take 10 . TMap.toList) dict)
        , env randomStrs $ \qs ->
            bench "match" (nf (\dict' -> map (`TMap.member` dict') qs) dict) ]
  , env (lenTMap <$> dictAmEn) $ \dict ->
      bgroup "traversal"
        [ bench "fmap" (nf (fmap (+3)) dict)
        , bench "foldMap" (nf (foldMap Sum) dict) ]
  , env (lenTMap <$> dictAmEn) $ \dictA ->
    env (lenTMap <$> dictBrEn) $ \dictB ->
    env (lenTMap <$> randomStrs) $ \dictSmall ->
      bgroup "combine"
        [ bench "union" (nf (uncurry TMap.union) (dictA, dictB))
        , bench "intersection" (nf (uncurry TMap.intersection) (dictA, dictB))
        , bench "difference" (nf (uncurry TMap.difference) (dictA, dictB))
        , bench "append" (nf (uncurry tmapProd) (dictSmall, dictSmall)) ]
  ]

lenTMap :: (Ord c) => [[c]] -> TMap c Int
lenTMap dict = TMap.fromList [(w, length w) | w <- dict]

tmapProd :: (Ord c) => TMap c Int -> TMap c Int -> TMap c (Sum Int)
tmapProd t1 t2 = TMap.appendWith (*) (Sum <$> t1) (Sum <$> t2)

benchMap :: Benchmark
benchMap = bgroup "Map" 
  [ bgroup "construction"
      [ env dictAmEnShuffled $ \dict ->
          bench "fromList" $ nf Map.fromList [(w, length w) | w <- dict]
      , env (sort <$> dictAmEn) $ \sortedDict ->
          bench "fromAscList" $ nf Map.fromAscList [(w, length w) | w <- sortedDict]
      ]
  , env (lenMap <$> dictAmEn) $ \dict ->
      bgroup "query"
        [ bench "isEmpty" (nf Map.null dict)
        , bench "stringCount" (nf Map.size dict)
        , bench "enumerate10" (nf (take 10 . Map.toList) dict)
        , env randomStrs $ \qs ->
            bench "match" (nf (\dict' -> map (`Map.member` dict') qs) dict) ]
  , env (lenMap <$> dictAmEn) $ \dict ->
      bgroup "traversal"
        [ bench "fmap" (nf (fmap (+3)) dict)
        , bench "foldMap" (nf (foldMap Sum) dict) ]
  , env (lenMap <$> dictAmEn) $ \dictA ->
    env (lenMap <$> dictBrEn) $ \dictB ->
    env (lenMap <$> randomStrs) $ \dictSmall ->
      bgroup "combine"
        [ bench "union" (nf (uncurry Map.union) (dictA, dictB))
        , bench "intersection" (nf (uncurry Map.intersection) (dictA, dictB))
        , bench "difference" (nf (uncurry Map.difference) (dictA, dictB))
        , bench "append" (nf (uncurry mapProd) (dictSmall, dictSmall)) ]
  ]

lenMap :: (Ord c) => [[c]] -> Map [c] Int
lenMap dict = Map.fromList [(w, length w) | w <- dict]

mapProd :: (Ord c) => Map [c] Int -> Map [c] Int -> Map [c] Int
mapProd m1 m2 =
  foldl' (Map.unionWith (+)) Map.empty
    [ prod1 s x m2 | (s,x) <- Map.toList m1 ]
  where
    prod1 s x m = Map.mapKeysMonotonic (s++) $ Map.map (x*) m

-------------------------------------------------------------------
-- Utility

revReplicateM :: (Monad m) => Int -> m a -> m [a]
revReplicateM n ma = loop n []
  where
    loop 0 acc = return acc
    loop i acc = ma >>= \a -> loop (i-1) (a:acc)
