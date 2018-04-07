module Data.Trie.MapSpec(
  spec
) where

import           Test.Hspec
import           Test.QuickCheck

import           Data.List (foldl')
import           Data.Map            (Map)
import qualified Data.Map            as Map
import           Data.Semigroup

import           Data.Trie.Map     as T
import           Data.Trie.Map.Gen

spec :: Spec
spec = do
  specify "toList empty = []" $
    toList (empty :: TMap C Int) `shouldSatisfy` Prelude.null
  specify "toList (singleton k a) = [(k,a)]" $
    property $ \k a -> toList (singleton (k :: [C]) (a :: Int)) === [(k,a)]
  specify "toAscList . fromList = Map.toAscList . Map.fromList" $
    property $ \strs -> (toAscList . fromList) (strs :: [([C], Int)]) ==
                        (Map.toAscList . Map.fromList) strs
  specify "null . toList = null" $
    property $ \(TMap' t) -> Prelude.null (toList t) === T.null t
  specify "length . toList = count" $
    property $ \(TMap' t) -> length (toList t) === count t
  
  specify "member k t = (k `Map.member` toMap t)" $
    property $ \(TMap' t) ->
      let strMap = toMap t
      in property $ \str -> member str t == Map.member str strMap
  specify "lookup k t = (k `Map.lookup` toMap t)" $
    property $ \(TMap' t) ->
      let strMap = toMap t
      in property $ \str -> T.lookup str t == Map.lookup str strMap
  
  specify "toMap (union a b) = Map.union (toMap a) (toMap b)" $
    property $ \(TMap' a) (TMap' b) ->
      toMap (union a b) == Map.union (toMap a) (toMap b)
  specify "toMap (unionWith f a b) = Map.unionWith f (toMap a) (toMap b)" $
    property $ \(Fn2 f) (TMap' a) (TMap' b) ->
      toMap (unionWith f a b) == Map.unionWith f (toMap a) (toMap b)
  specify "toMap (intersection a b) = Map.intersection (toMap a) (toMap b)" $
    property $ \(TMap' a) (TMap' b) ->
      toMap (intersection a b) == Map.intersection (toMap a) (toMap b)
  specify "toMap (difference a b) = Map.difference (toMap a) (toMap b)" $
    property $ \(TMap' a) (TMap' b) ->
      toMap (difference a b) == Map.difference (toMap a) (toMap b)
  specify "toMap (append a b) = mapAppend (toMap a) (toMap b)" $
    property $ \(TMap' a) (TMap' b) ->
      toMap (getSum <$> appendWith (\x y -> Sum (x * y)) a b) ==
      mapAppend (toMap a) (toMap b)
      
  specify "validTMap (union a b)" $
    property $ \(TMap' a) (TMap' b) ->
      validTMap (union a b)
  specify "validTMap (intersection a b)" $
    property $ \(TMap' a) (TMap' b) ->
      validTMap (intersection a b)
  specify "validTMap (difference a b)" $
    property $ \(TMap' a) (TMap' b) ->
      validTMap (difference a b)
  specify "validTMap (append a b)" $
    property $ \(TMap' a) (TMap' b) ->
      validTMap (getSum <$> appendWith (\x y -> Sum (x * y)) a b)

mapAppend :: (Ord c) => Map [c] Int -> Map [c] Int -> Map [c] Int
mapAppend ass bss =
  sumUnions
    [ Map.mapKeysMonotonic (as ++) $ Map.map (v *) bss
      | (as, v) <- Map.toAscList ass ]
  where
    sumUnions = foldl' (Map.unionWith (+)) Map.empty
