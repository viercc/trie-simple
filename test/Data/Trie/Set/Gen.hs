module Data.Trie.Set.Gen(
  C(..),
  TSet'(..),
  genTSet,
  acceptStrs
) where

import           Test.QuickCheck hiding (shrinkMapBy)

import           Data.Trie.Set
import           Common

newtype TSet' = TSet' (TSet C)

instance Show TSet' where
  show (TSet' t) = show t

instance Arbitrary TSet' where
  arbitrary = TSet' <$> genTSet 
  shrink (TSet' t) = TSet' <$> shrinkTSet t

genTSet :: (Ord c, Arbitrary c) => Gen (TSet c)
genTSet = strings <$> arbitrary

shrinkTSet :: (Ord c, Arbitrary c) => TSet c -> [TSet c]
shrinkTSet (TSet (Node a e)) =
  [ TSet (Node False e) | a ] ++
  [ TSet (Node a e') | e' <- shrinkMapBy shrinkTSet e ]

acceptStrs :: TSet c -> Gen [[c]]
acceptStrs t = sized $ \n ->
  let m = count t
      loop _ [] = return []
      loop k (a:as)
        | k <= 0    = return []
        | otherwise =
            frequency [(n, (a:) <$> loop (k-1) as), (m, loop k as)]
  in loop n (enumerate t)
