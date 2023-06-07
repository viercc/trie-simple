{-# LANGUAGE RecordWildCards #-}
module Main(main) where

import Gauge.Main

import qualified Data.Trie.Set as TSet
import Data.Trie.Map (TMap)
import qualified Data.Trie.Map as TMap

import Data.Monoid
import Data.List (inits, tails, foldl')
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map

import Common

main :: IO ()
main = defaultMain
  [ benchAll englishDataset "English"
  , benchAll wikiDataset "Wiki"
  ]

benchAll :: IO Dataset -> String -> Benchmark
benchAll setupEnv groupName =
  env setupEnv $ \dataset ->
    bgroup groupName [
      benchSet dataset,
      benchTSet dataset,
      benchMap dataset,
      benchTMap dataset
      ]

benchTSet :: Dataset -> Benchmark
benchTSet ~Dataset{..} = bgroup "TSet" 
  [ bgroup "construction"
      [ bench "fromList" $ whnf TSet.fromList dictAUnsorted
      , bench "fromAscList" $ whnf TSet.fromAscList dictA ]
  , env (pure $ TSet.fromList dictA) $ \mapA ->
      bgroup "query"
        [ bench "isEmpty" (nf TSet.null mapA)
        , bench "stringCount" (nf TSet.count mapA)
        , bench "enumerate10" (nf (take 10 . TSet.enumerate) mapA)
        , bench "enumerateAll" (nf TSet.enumerate mapA)
        , bench "match" (nf (\dict' -> map (`TSet.member` dict') gibberish) mapA) ]
  , env (pure (TSet.fromList dictA)) $ \mapA ->
      bgroup "single-item"
        [ bench "insert1" (whnf (TSet.insert "######fake_key######") mapA)
        , bench "insert2" (whnf (TSet.insert realKey) mapA)
        , bench "delete1" (whnf (TSet.delete "######fake_key######") mapA)
        , bench "delete2" (whnf (TSet.delete realKey) mapA)
        ]
  , env (pure $ TSet.fromList dictA) $ \mapA ->
    env (pure $ TSet.fromList dictB) $ \mapB ->
      bgroup "combine"
        [ bench "union" (whnf (uncurry TSet.union) (mapA, mapB))
        , bench "intersection" (whnf (uncurry TSet.intersection) (mapA, mapB))
        , bench "difference" (whnf (uncurry TSet.difference) (mapA, mapB))
        , env (pure $ TSet.fromList gibberish) $ \mapSmall ->
            bench "append" (whnf (uncurry TSet.append) (mapSmall, mapSmall))
        , bench "prefixes" (whnf TSet.prefixes mapA)
        , bench "suffixes" (whnf TSet.suffixes mapB) ]
  ]
  where realKey = head dictA

benchSet :: Dataset -> Benchmark
benchSet ~Dataset{..} = bgroup "Set"
  [ bgroup "construction"
      -- Set.fromList detects whether the input list is sorted
      -- and switch the algorithm based on it.
      -- Using shuffled dictionary avoids this optimization fires
      -- in this benchmark.
      [ bench "fromList" $ whnf Set.fromList dictAUnsorted
      , bench "fromAscList" $ whnf Set.fromAscList dictA ]
  , env (pure $ Set.fromList dictA) $ \mapA ->
      bgroup "query"
        [ bench "isEmpty" (nf Set.null mapA)
        , bench "stringCount" (nf Set.size mapA)
        , bench "enumerate10" (nf (take 10 . Set.toList) mapA)
        , bench "enumerateAll" (nf Set.toList mapA)
        , bench "match" (nf (\dict' -> map (`Set.member` dict') gibberish) mapA) ]
  , env (pure (Set.fromList dictA)) $ \mapA ->
      bgroup "single-item"
        [ bench "insert1" (whnf (Set.insert "######fake_key######") mapA)
        , bench "insert2" (whnf (Set.insert realKey) mapA)
        , bench "delete1" (whnf (Set.delete "######fake_key######") mapA)
        , bench "delete2" (whnf (Set.delete realKey) mapA)
        ]
  , env (pure $ Set.fromList dictA) $ \mapA ->
    env (pure $ Set.fromList dictB) $ \mapB ->
      bgroup "combine"
        [ bench "union" (whnf (uncurry Set.union) (mapA, mapB))
        , bench "intersection" (whnf (uncurry Set.intersection) (mapA, mapB))
        , bench "difference" (whnf (uncurry Set.difference) (mapA, mapB))
        , env (pure $ Set.fromList gibberish) $ \mapSmall ->
            bench "append" (whnf (uncurry setAppend) (mapSmall, mapSmall))
        , bench "prefixes" (whnf setPrefixes mapA)
        , bench "suffixes" (whnf setSuffixes mapB) ]
  ]
  where realKey = head dictA

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


benchTMap :: Dataset -> Benchmark
benchTMap ~Dataset{..} = bgroup "TMap" 
  [ bgroup "construction"
      [ bench "fromList" $ whnf TMap.fromList [(w, length w) | w <- dictAUnsorted ]
      , bench "fromAscList" $ whnf TMap.fromAscList [(w, length w) | w <- dictA ]
      ]
  , env (pure $ lenTMap dictA) $ \mapA ->
      bgroup "query"
        [ bench "isEmpty" (nf TMap.null mapA)
        , bench "stringCount" (nf TMap.count mapA)
        , bench "enumerate10" (nf (take 10 . TMap.toList) mapA)
        , bench "match" (nf (\dict' -> map (`TMap.member` dict') gibberish) mapA) ]
  , env (pure (lenTMap dictA)) $ \mapA ->
      bgroup "single-item"
        [ bench "insert1" (whnf (TMap.insert "######fake_key######" 1) mapA)
        , bench "insert2" (whnf (TMap.insert realKey 1) mapA)
        , bench "delete1" (whnf (TMap.delete "######fake_key######") mapA)
        , bench "delete2" (whnf (TMap.delete realKey) mapA)
        , bench "alter1" (whnf (TMap.alter alterFn "######fake_key######") mapA)
        , bench "alter2" (whnf (TMap.alter alterFn realKey) mapA)
        ]
  , env (pure (lenTMap dictA)) $ \mapA ->
      bgroup "traversal"
        [ bench "fmap" (nf (fmap (+3)) mapA)
        , bench "foldMap" (nf (foldMap Sum) mapA) ]
  , env (pure $ lenTMap dictA) $ \mapA ->
    env (pure $ lenTMap dictB) $ \mapB ->
      bgroup "combine"
        [ bench "union" (whnf (uncurry TMap.union) (mapA, mapB))
        , bench "intersection" (whnf (uncurry TMap.intersection) (mapA, mapB))
        , bench "difference" (whnf (uncurry TMap.difference) (mapA, mapB))
        , env (pure $ lenTMap gibberish) $ \mapSmall ->
            bench "append" (whnf (uncurry tmapProd) (mapSmall, mapSmall)) ]
  ]
  where realKey = head dictA

alterFn :: Maybe Int -> Maybe Int
alterFn Nothing = Just 1000
alterFn (Just a) = if even a then Just a else Nothing

lenTMap :: (Ord c) => [[c]] -> TMap c Int
lenTMap dict = TMap.fromList [(w, length w) | w <- dict]

tmapProd :: (Ord c) => TMap c Int -> TMap c Int -> TMap c (Sum Int)
tmapProd = TMap.appendWith (\x y -> Sum (x * y))

benchMap :: Dataset -> Benchmark
benchMap ~Dataset{..} = bgroup "Map" 
  [ bgroup "construction"
      [ bench "fromList" $ whnf Map.fromList [(w, length w) | w <- dictAUnsorted ]
      , bench "fromAscList" $ whnf Map.fromAscList [(w, length w) | w <- dictA ]
      ]
  , env (pure $ lenMap dictA) $ \mapA ->
      bgroup "query"
        [ bench "isEmpty" (nf Map.null mapA)
        , bench "stringCount" (nf Map.size mapA)
        , bench "enumerate10" (nf (take 10 . Map.toList) mapA)
        , bench "match" (nf (\dict' -> map (`Map.member` dict') gibberish) mapA) ]
  , env (pure (lenMap dictA)) $ \mapA ->
      bgroup "single-item"
        [ bench "insert1" (whnf (Map.insert "######fake_key######" 1) mapA)
        , bench "insert2" (whnf (Map.insert realKey 1) mapA)
        , bench "delete1" (whnf (Map.delete "######fake_key######") mapA)
        , bench "delete2" (whnf (Map.delete realKey) mapA)
        , bench "alter1" (whnf (Map.alter alterFn "######fake_key######") mapA)
        , bench "alter2" (whnf (Map.alter alterFn realKey) mapA)
        ]
  , env (pure (lenMap dictA)) $ \mapA ->
      bgroup "traversal"
        [ bench "fmap" (nf (fmap (+3)) mapA)
        , bench "foldMap" (nf (foldMap Sum) mapA) ]
  , env (pure $ lenMap dictA) $ \mapA ->
    env (pure $ lenMap dictB) $ \mapB ->
      bgroup "combine"
        [ bench "union" (whnf (uncurry Map.union) (mapA, mapB))
        , bench "intersection" (whnf (uncurry Map.intersection) (mapA, mapB))
        , bench "difference" (whnf (uncurry Map.difference) (mapA, mapB))
        , env (pure $ lenMap gibberish) $ \mapSmall ->
            bench "append" (whnf (uncurry mapProd) (mapSmall, mapSmall)) ]
  ]
  where realKey = head dictA

lenMap :: (Ord c) => [[c]] -> Map [c] Int
lenMap dict = Map.fromList [(w, length w) | w <- dict]

mapProd :: (Ord c) => Map [c] Int -> Map [c] Int -> Map [c] Int
mapProd m1 m2 =
  foldl' (Map.unionWith (+)) Map.empty
    [ prod1 s x m2 | (s,x) <- Map.toList m1 ]
  where
    prod1 s x m = Map.mapKeysMonotonic (s++) $ Map.map (x*) m
