module Data.Trie.Map.Gen(
  C(..), U(..)
) where

import           Test.QuickCheck hiding (shrinkMapBy)
import           Test.QuickCheck.Checkers

import Data.Maybe (isJust)
import qualified Data.Map.Lazy as Map

import           Data.Trie.Map
import           Data.Trie.Map.Internal
import           Common

data U = U
  deriving (Eq, Ord, Enum)

instance Show U where
  showsPrec _ U = showString "U"
  showList us = showString $ "U^" ++ show (length us)

instance EqProp U where
  (=-=) = eq

instance Arbitrary U where
  arbitrary = return U

instance (Ord c, Arbitrary c, Arbitrary a) => Arbitrary (TMap c a) where
  arbitrary = genTMap
  shrink    = shrinkTMap

instance (EqProp c, EqProp a) => EqProp (TMap c a) where
  x =-= y  = toList x =-= toList y

instance EqProp C where
  (=-=) = eq

genTMap :: (Ord c, Arbitrary c, Arbitrary a) => Gen (TMap c a)
genTMap = fromList <$> arbitrary

shrinkTMap :: (Ord c, Arbitrary c, Arbitrary a) => TMap c a -> [TMap c a]
shrinkTMap (TMap (Node ma e)) = filter validTMap $ 
  Map.elems e ++
  [ TMap (Node Nothing e) | isJust ma] ++
  [ TMap (Node ma e') | e' <- shrinkMapBy shrinkTMap e ] ++
  [ TMap (Node (Just a') e) | Just a <- [ma], a' <- shrink a ]

validTMap :: TMap c a -> Bool
validTMap t = Data.Trie.Map.null t == Prelude.null (elems t)
