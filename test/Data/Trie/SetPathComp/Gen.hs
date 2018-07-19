module Data.Trie.SetPathComp.Gen(
  C(..),
  TSet'(..),
  TSet''(..),
  genTSet,
  acceptStrs,
  validTSet
) where

import           Test.QuickCheck hiding (shrinkMapBy)

import qualified Data.Map as Map
import           Data.Trie.SetPathComp
import           Common

newtype TSet' = TSet' (TSet C)
newtype TSet'' = TSet'' (TSet B)

instance Show TSet' where
  show (TSet' t) = show t

instance Arbitrary TSet' where
  arbitrary = TSet' <$> genTSet 
  shrink (TSet' t) = TSet' <$> shrinkTSet t

instance Show TSet'' where
  show (TSet'' t) = show t

instance Arbitrary TSet'' where
  arbitrary = TSet'' <$> genTSet 
  shrink (TSet'' t) = TSet'' <$> shrinkTSet t

genTSet :: (Ord c, Arbitrary c) => Gen (TSet c)
genTSet = fromList <$> arbitrary

shrinkTSet :: (Ord c, Arbitrary c) => TSet c -> [TSet c]
shrinkTSet (TSet (Node cs a e)) = filter validTSet $
  [ TSet (Node cs False e) | a ] ++
  [ TSet (Node cs a e') | e' <- shrinkMapBy shrinkTSet e ]

acceptStrs :: TSet c -> Gen [[c]]
acceptStrs t = sized $ \n ->
  let m = count t
      loop _ [] = return []
      loop k (a:as)
        | k <= 0    = return []
        | otherwise =
            frequency [(n, (a:) <$> loop (k-1) as), (m, loop k as)]
  in loop n (enumerate t)

validTSet :: TSet c -> Bool
validTSet = snd . foldTSet step
  where
    step (Node _ a e) =
      let isEmpty = not a && all fst e
          isValid = (not isEmpty || Map.null e) && all snd e
      in (isEmpty, isValid)
