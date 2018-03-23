module Data.Trie.Map.Gen(
  C(..),
  TMap'(..),
  genTMap,
) where

import           Test.QuickCheck hiding (shrinkMapBy)

import           Data.Trie.Map
import           Common

newtype TMap' = TMap' (TMap C Int)

instance Show TMap' where
  show (TMap' t) = show t

instance Arbitrary TMap' where
  arbitrary = TMap' <$> genTMap 
  shrink (TMap' t) = TMap' <$> shrinkTMap t

genTMap :: (Ord c, Arbitrary c, Arbitrary a) => Gen (TMap c a)
genTMap = fromList <$> arbitrary

shrinkTMap :: (Ord c, Arbitrary c, Arbitrary a) => TMap c a -> [TMap c a]
shrinkTMap (TMap (Node ma e)) =
  [ TMap (Node Nothing e) | Just _ <- [ma] ] ++
  [ TMap (Node (Just a') e) | Just a <- [ma], a' <- shrink a ] ++
  [ TMap (Node ma e') | e' <- shrinkMapBy shrinkTMap e ]
