module Data.Trie.SetSpec(
  spec
) where

import           Test.Hspec
import           Test.QuickCheck

import           Data.List (inits, tails)
import           Data.Set            (Set)
import qualified Data.Set            as Set

import           Data.Trie.Set     as T
import           Data.Trie.Set.Gen

spec :: Spec
spec = do
  specify "enumerate empty = []" $
    enumerate (empty :: TSet C) `shouldSatisfy` Prelude.null
  specify "enumerate . string = (:[])" $
    property $ \str -> enumerate (string (str :: [C])) === [str]
  specify "enumerate . strings = Set.toAscList . Set.fromList" $
    property $ \strs -> (enumerate . strings) (strs :: [[C]]) == (Set.toAscList . Set.fromList) strs
  specify "null . enumerate = null" $
    property $ \(TSet' t) -> Prelude.null (enumerate t) === T.null t
  specify "length . enumerate = count" $
    property $ \(TSet' t) -> length (enumerate t) === count t
  
  specify "member t = (`Set.member` toSet t)" $
    property $ \(TSet' t) ->
      let strSet = toSet t
      in property $ \str -> member str t == Set.member str strSet
  specify "forAll (str `in` enumerate t). member str t" $
    property $ \(TSet' t) -> all (`member` t) <$> acceptStrs t
    
  specify "toSet (union a b) = Set.union (toSet a) (toSet b)" $
    property $ \(TSet' a) (TSet' b) ->
      toSet (union a b) == Set.union (toSet a) (toSet b)
  specify "toSet (intersection a b) = Set.intersection (toSet a) (toSet b)" $
    property $ \(TSet' a) (TSet' b) ->
      toSet (intersection a b) == Set.intersection (toSet a) (toSet b)
  specify "toSet (difference a b) = Set.difference (toSet a) (toSet b)" $
    property $ \(TSet' a) (TSet' b) ->
      toSet (difference a b) == Set.difference (toSet a) (toSet b)
  specify "toSet (append a b) = setAppend (toSet a) (toSet b)" $
    property $ \(TSet' a) (TSet' b) ->
      toSet (append a b) == setAppend (toSet a) (toSet b)
  
  specify "toSet (prefixes a) = setPrefixes (toSet a)" $
    property $ \(TSet' a) ->
      toSet (prefixes a) === setPrefixes (toSet a)
  specify "toSet (suffixes a) = setSuffixes (toSet a)" $
    property $ \(TSet' a) ->
      toSet (suffixes a) === setSuffixes (toSet a)

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
