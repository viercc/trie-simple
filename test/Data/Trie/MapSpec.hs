module Data.Trie.MapSpec(
  spec
) where

import           Test.Hspec
import           Test.QuickCheck

import           Data.List (sortBy, foldl')
import           Data.Ord
import           Data.Map            (Map)
import qualified Data.Map            as Map
import           Data.Semigroup

import           Data.Trie.Map     as T
import           Data.Trie.Map.Gen
import qualified Data.Trie.Set     as TSet
import           Data.Trie.Set.Gen

spec :: Spec
spec = do
  specify "toList empty = []" $
    toList (empty :: TMap C Int) `shouldSatisfy` Prelude.null
  specify "toList (just a) = [([], a)]" $
    property $ \a -> toList (just a :: TMap C Int) === [([], a)]
  specify "toList (singleton k a) = [(k,a)]" $
    property $ \k a -> toList (singleton k a :: TMap C Int) === [(k,a)]
  specify "toAscList . fromList = Map.toAscList . Map.fromList" $
    property $ \strs -> toAscList (fromList strs :: TMap C Int) ==
                        (Map.toAscList . Map.fromList) strs
  specify "fromAscList . sortBy (comparing fst) = fromList" $
    property $ \strs -> fromAscList (sortBy (comparing fst) strs) ==
                        (fromList strs :: TMap C Int)
  specify "fromMap . Map.fromList = fromList" $
    property $ \strs -> fromMap (Map.fromList strs) ==
                        (fromList strs :: TMap C Int)
  specify "null . toList = null" $
    property $ \(TMap' t) -> Prelude.null (toList t) === T.null t
  specify "length . toList = count" $
    property $ \(TMap' t) -> length (toList t) === count t
  specify "keys = map fst . toList" $
    property $ \(TMap' t) -> keys t == map fst (toList t)
  specify "elems = map snd . toList" $
    property $ \(TMap' t) -> elems t == map snd (toList t)
  specify "keysTSet = TSet.fromAscList . keys" $
    property $ \(TMap' t) -> keysTSet t == TSet.fromAscList (keys t)
  specify "fromTSet id = fromAscList . map (\\k -> (k,k)) . TSet.toAscList" $
    property $ \(TSet' t) ->
      fromTSet id t == fromAscList (map (\k -> (k,k)) (TSet.toAscList t))
  
  specify "member k t = (k `Map.member` toMap t)" $
    property $ \(TMap'' t) ->
      let strMap = toMap t
      in property $ \str -> member str t == Map.member str strMap
  specify "lookup k t = (k `Map.lookup` toMap t)" $
    property $ \(TMap'' t) ->
      let strMap = toMap t
      in property $ \str -> T.lookup str t == Map.lookup str strMap
  specify "lookup (k ++ l) t == lookup l (snd (match k t))" $
    property $ \k l (TMap'' t) ->
      T.lookup (k ++ l) t == T.lookup l (snd (T.match k t))

  specify "toMap (insert k 1 a) = Map.insert k 1 (toMap a)" $
    property $ \k (TMap'' a) ->
      toMap (insert k 1 a) == Map.insert k 1 (toMap a)
  specify "toMap (delete k a) = Map.delete k (toMap a)" $
    property $ \k (TMap'' a) ->
      toMap (delete k a) == Map.delete k (toMap a)
  specify "toMap (insertWith (+) k 1 a) = Map.insertWith (+) k 1 (toMap a)" $
    property $ \k (TMap'' a) ->
      toMap (insertWith (+) k 1 a) ==
      Map.insertWith (+) k 1 (toMap a)
  let sub b a = if a < b then Nothing else Just (a - b)
  specify "toMap (deleteWith sub k 1 a) = Map.update (sub 1) k (toMap a)" $
    property $ \k (TMap'' a) ->
      toMap (deleteWith sub k 1 a) ==
      Map.update (sub 1) k (toMap a)
  specify "toMap (adjust (*2) k a) = Map.adjust (*2) k (toMap a)" $
    property $ \k (TMap'' a) ->
      toMap (adjust (*2) k a) == Map.adjust (*2) k (toMap a)
  let alterer Nothing = Just 100
      alterer (Just a) | odd a = Nothing
                       | otherwise = Just (a `div` 2)
  specify "toMap (alter alterer k a) = Map.alter alterer k (toMap a)" $
    property $ \k (TMap'' a) ->
      toMap (alter alterer k a) == Map.alter alterer k (toMap a)

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
