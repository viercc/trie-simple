module Main(main) where

import Criterion.Main

import qualified Data.Trie.Set as TSet
import qualified Data.Trie.SetPathComp as TSetPC
import Data.Trie.Map (TMap)
import qualified Data.Trie.Map as TMap

import Data.Monoid
import Data.List (sort, inits, tails, foldl')
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map

import Common

main :: IO ()
main = defaultMain [ benchTSet, benchTSetPC, benchSet, benchTMap, benchMap ]

benchTSetPC :: Benchmark
benchTSetPC = bgroup "TSetPC" 
  [ bgroup "construction"
      [ env dictAmEnShuffled $ \dict ->
          bench "fromList" $ whnf TSetPC.fromList dict
      , env (sort <$> dictAmEn) $ \sortedDict ->
          bench "fromAscList" $ whnf TSetPC.fromAscList sortedDict
      , bench "fromList_stream" $ whnfIO (TSetPC.fromList <$> dictAmEn) ]
  , env (TSetPC.fromList <$> dictAmEn) $ \dict ->
      bgroup "query"
        [ bench "isEmpty" (nf TSetPC.null dict)
        , bench "stringCount" (nf TSetPC.count dict)
        , bench "enumerate10" (nf (take 10 . TSetPC.enumerate) dict)
        , bench "enumerateAll" (nf TSetPC.enumerate dict)
        , env randomStrs $ \qs ->
            bench "match" (nf (\dict' -> map (`TSetPC.member` dict') qs) dict) ]
  , env (TSetPC.fromList <$> dictAmEn) $ \dict ->
      bgroup "single-item"
        [ bench "insert1" (whnf (TSetPC.insert "wwwwwwwwwwwwwwww") dict)
        , bench "insert2" (whnf (TSetPC.insert "cheese") dict)
        , bench "delete1" (whnf (TSetPC.delete "wwwwwwwwwwwwwwww") dict)
        , bench "delete2" (whnf (TSetPC.delete "cheese") dict)
        ]
  , env (TSetPC.fromList <$> dictAmEn) $ \dictA ->
    env (TSetPC.fromList <$> dictBrEn) $ \dictB ->
    env (TSetPC.fromList <$> randomStrs) $ \dictSmall ->
      bgroup "combine"
        [ bench "union" (whnf (uncurry TSetPC.union) (dictA, dictB))
        , bench "intersection" (whnf (uncurry TSetPC.intersection) (dictA, dictB))
        , bench "difference" (whnf (uncurry TSetPC.difference) (dictA, dictB)) ]
  ]

benchTSet :: Benchmark
benchTSet = bgroup "TSet" 
  [ bgroup "construction"
      [ env dictAmEnShuffled $ \dict ->
          bench "fromList" $ whnf TSet.fromList dict
      , env (sort <$> dictAmEn) $ \sortedDict ->
          bench "fromAscList" $ whnf TSet.fromAscList sortedDict
      , bench "fromList_stream" $ whnfIO (TSet.fromList <$> dictAmEn) ]
  , env (TSet.fromList <$> dictAmEn) $ \dict ->
      bgroup "query"
        [ bench "isEmpty" (nf TSet.null dict)
        , bench "stringCount" (nf TSet.count dict)
        , bench "enumerate10" (nf (take 10 . TSet.enumerate) dict)
        , bench "enumerateAll" (nf TSet.enumerate dict)
        , env randomStrs $ \qs ->
            bench "match" (nf (\dict' -> map (`TSet.member` dict') qs) dict) ]
  , env (TSet.fromList <$> dictAmEn) $ \dict ->
      bgroup "single-item"
        [ bench "insert1" (whnf (TSet.insert "wwwwwwwwwwwwwwww") dict)
        , bench "insert2" (whnf (TSet.insert "cheese") dict)
        , bench "delete1" (whnf (TSet.delete "wwwwwwwwwwwwwwww") dict)
        , bench "delete2" (whnf (TSet.delete "cheese") dict)
        ]
  , env (TSet.fromList <$> dictAmEn) $ \dictA ->
    env (TSet.fromList <$> dictBrEn) $ \dictB ->
    env (TSet.fromList <$> randomStrs) $ \dictSmall ->
      bgroup "combine"
        [ bench "union" (whnf (uncurry TSet.union) (dictA, dictB))
        , bench "intersection" (whnf (uncurry TSet.intersection) (dictA, dictB))
        , bench "difference" (whnf (uncurry TSet.difference) (dictA, dictB))
        , bench "append" (whnf (uncurry TSet.append) (dictSmall, dictSmall))
        , bench "prefixes" (whnf TSet.prefixes dictA)
        , bench "suffixes" (whnf TSet.suffixes dictB) ]
  ]

benchSet :: Benchmark
benchSet = bgroup "Set" 
  [ bgroup "construction"
      -- Set.fromList detects whether the input list is sorted
      -- and switch the algorithm based on it.
      -- Using shuffled dictionary avoids this optimization fires
      -- in this benchmark.
      [ env dictAmEnShuffled $ \dict ->
          bench "fromList" $ whnf Set.fromList dict
      , env (sort <$> dictAmEn) $ \sortedDict ->
          bench "fromAscList" $ whnf Set.fromAscList sortedDict
      , bench "fromList_stream" $ whnfIO (TSet.fromList <$> dictAmEn) ]
  , env (Set.fromList <$> dictAmEn) $ \dictSet ->
      bgroup "query"
        [ bench "isEmpty" (nf Set.null dictSet)
        , bench "stringCount" (nf Set.size dictSet)
        , bench "enumerate10" (nf (take 10 . Set.toList) dictSet)
        , bench "enumerateAll" (nf Set.toList dictSet)
        , env randomStrs $ \qs ->
            bench "match" (nf (\dictSet' -> map (`Set.member` dictSet') qs) dictSet) ]
  , env (Set.fromList <$> dictAmEn) $ \dict ->
      bgroup "single-item"
        [ bench "insert1" (whnf (Set.insert "wwwwwwwwwwwwwwww") dict)
        , bench "insert2" (whnf (Set.insert "cheese") dict)
        , bench "delete1" (whnf (Set.delete "wwwwwwwwwwwwwwww") dict)
        , bench "delete2" (whnf (Set.delete "cheese") dict)
        ]
  , env (Set.fromList <$> dictAmEn) $ \dictA ->
    env (Set.fromList <$> dictBrEn) $ \dictB ->
    env (Set.fromList <$> randomStrs) $ \dictSmall ->
      bgroup "combine"
        [ bench "union" (whnf (uncurry Set.union) (dictA, dictB))
        , bench "intersection" (whnf (uncurry Set.intersection) (dictA, dictB))
        , bench "difference" (whnf (uncurry Set.difference) (dictA, dictB))
        , bench "append" (whnf (uncurry setAppend) (dictSmall, dictSmall))
        , bench "prefixes" (whnf setPrefixes dictA)
        , bench "suffixes" (whnf setSuffixes dictB) ]
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
          bench "fromList" $ whnf TMap.fromList [(w, length w) | w <- dict]
      , env (sort <$> dictAmEn) $ \sortedDict ->
          bench "fromAscList" $ whnf TMap.fromAscList [(w, length w) | w <- sortedDict]
      ]
  , env (lenTMap <$> dictAmEn) $ \dict ->
      bgroup "query"
        [ bench "isEmpty" (nf TMap.null dict)
        , bench "stringCount" (nf TMap.count dict)
        , bench "enumerate10" (nf (take 10 . TMap.toList) dict)
        , env randomStrs $ \qs ->
            bench "match" (nf (\dict' -> map (`TMap.member` dict') qs) dict) ]
  , env (lenTMap <$> dictAmEn) $ \dict ->
      bgroup "single-item"
        [ bench "insert1" (whnf (TMap.insert "wwwwwwwwwwwwwwww" 1) dict)
        , bench "insert2" (whnf (TMap.insert "cheese" 1) dict)
        , bench "delete1" (whnf (TMap.delete "wwwwwwwwwwwwwwww") dict)
        , bench "delete2" (whnf (TMap.delete "cheese") dict)
        , bench "alter1" (whnf (TMap.alter alterFn "wwwwwwwwwwwwwwww") dict)
        , bench "alter2" (whnf (TMap.alter alterFn "cheese") dict)
        ]
  , env (lenTMap <$> dictAmEn) $ \dict ->
      bgroup "traversal"
        [ bench "fmap" (nf (fmap (+3)) dict)
        , bench "foldMap" (nf (foldMap Sum) dict) ]
  , env (lenTMap <$> dictAmEn) $ \dictA ->
    env (lenTMap <$> dictBrEn) $ \dictB ->
    env (lenTMap <$> randomStrs) $ \dictSmall ->
      bgroup "combine"
        [ bench "union" (whnf (uncurry TMap.union) (dictA, dictB))
        , bench "intersection" (whnf (uncurry TMap.intersection) (dictA, dictB))
        , bench "difference" (whnf (uncurry TMap.difference) (dictA, dictB))
        , bench "append" (whnf (uncurry tmapProd) (dictSmall, dictSmall)) ]
  ]

alterFn :: Maybe Int -> Maybe Int
alterFn Nothing = Just 1000
alterFn (Just a) = if even a then Just a else Nothing

lenTMap :: (Ord c) => [[c]] -> TMap c Int
lenTMap dict = TMap.fromList [(w, length w) | w <- dict]

tmapProd :: (Ord c) => TMap c Int -> TMap c Int -> TMap c (Sum Int)
tmapProd t1 t2 = TMap.appendWith (*) (Sum <$> t1) (Sum <$> t2)

benchMap :: Benchmark
benchMap = bgroup "Map" 
  [ bgroup "construction"
      [ env dictAmEnShuffled $ \dict ->
          bench "fromList" $ whnf Map.fromList [(w, length w) | w <- dict]
      , env (sort <$> dictAmEn) $ \sortedDict ->
          bench "fromAscList" $ whnf Map.fromAscList [(w, length w) | w <- sortedDict]
      ]
  , env (lenMap <$> dictAmEn) $ \dict ->
      bgroup "query"
        [ bench "isEmpty" (nf Map.null dict)
        , bench "stringCount" (nf Map.size dict)
        , bench "enumerate10" (nf (take 10 . Map.toList) dict)
        , env randomStrs $ \qs ->
            bench "match" (nf (\dict' -> map (`Map.member` dict') qs) dict) ]
  , env (lenMap <$> dictAmEn) $ \dict ->
      bgroup "single-item"
        [ bench "insert1" (whnf (Map.insert "wwwwwwwwwwwwwwww" 1) dict)
        , bench "insert2" (whnf (Map.insert "cheese" 1) dict)
        , bench "delete1" (whnf (Map.delete "wwwwwwwwwwwwwwww") dict)
        , bench "delete2" (whnf (Map.delete "cheese") dict)
        , bench "alter1" (whnf (Map.alter alterFn "wwwwwwwwwwwwwwww") dict)
        , bench "alter2" (whnf (Map.alter alterFn "cheese") dict)
        ]
  , env (lenMap <$> dictAmEn) $ \dict ->
      bgroup "traversal"
        [ bench "fmap" (nf (fmap (+3)) dict)
        , bench "foldMap" (nf (foldMap Sum) dict) ]
  , env (lenMap <$> dictAmEn) $ \dictA ->
    env (lenMap <$> dictBrEn) $ \dictB ->
    env (lenMap <$> randomStrs) $ \dictSmall ->
      bgroup "combine"
        [ bench "union" (whnf (uncurry Map.union) (dictA, dictB))
        , bench "intersection" (whnf (uncurry Map.intersection) (dictA, dictB))
        , bench "difference" (whnf (uncurry Map.difference) (dictA, dictB))
        , bench "append" (whnf (uncurry mapProd) (dictSmall, dictSmall)) ]
  ]

lenMap :: (Ord c) => [[c]] -> Map [c] Int
lenMap dict = Map.fromList [(w, length w) | w <- dict]

mapProd :: (Ord c) => Map [c] Int -> Map [c] Int -> Map [c] Int
mapProd m1 m2 =
  foldl' (Map.unionWith (+)) Map.empty
    [ prod1 s x m2 | (s,x) <- Map.toList m1 ]
  where
    prod1 s x m = Map.mapKeysMonotonic (s++) $ Map.map (x*) m
