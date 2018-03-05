{-# LANGUAGE DeriveFunctor #-}
module Common where

import           Test.QuickCheck

import           Control.Applicative
import           Data.List          (tails)
import           Data.Map           (Map)
import qualified Data.Map.Strict as Map

newtype C = C { unC :: Char }
  deriving (Eq, Ord)

instance Show C where
    showsPrec p (C c) = showsPrec p c
    showList cs = showList (map unC cs)

instance Arbitrary C where
    arbitrary = sized $ \n ->
        let m = sqrti (max 1 n)
        in C <$> elements (take m "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

sqrti :: Int -> Int
sqrti = floor . (sqrt :: Double -> Double) . fromIntegral

randomPair :: [a] -> Gen (a,a)
randomPair as =
  let mkP [] = []
      mkP [_] = []
      mkP (x:xs) = (,) x <$> xs
      ps = tails as >>= mkP
  in elements ps

data Once a = Once { getDefault :: a, getVariants :: [a] }
  deriving (Functor)

instance Applicative Once where
    pure a = Once a []
    Once f fs <*> Once a as = Once (f a) (fs <*> pure a <|> f <$> as)

shrinkTraversableBy :: (Traversable t) => (a -> [a]) -> t a -> [t a]
shrinkTraversableBy f = getVariants . traverse (\a -> Once a (f a))

shrinkMapBy :: (Ord k) => (a -> [a]) -> Map k a -> [Map k a]
shrinkMapBy f = map Map.fromList . shrinkList f' . Map.toList
  where f' (k, a) = (,) k <$> f a
