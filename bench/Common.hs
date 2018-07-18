module Common(
  dictAmEn, dictBrEn,
  dictAmEnShuffled, randomStrs
) where

import qualified Data.Vector as V
import Data.Word

import qualified System.Random.MWC                as R
import qualified System.Random.MWC.CondensedTable as R
import qualified System.Random.MWC.Distributions  as R

numRandomStr :: Int
numRandomStr = 1000

seed :: Word32 -> V.Vector Word32
seed w = V.fromList [1573289798, 32614861, w]

dictAmEn, dictBrEn, dictAmEnShuffled, randomStrs :: IO [String]
dictAmEn = lines <$> readFile "/usr/share/dict/american-english"
dictBrEn = lines <$> readFile "/usr/share/dict/british-english"
dictAmEnShuffled =
  do g <- R.initialize (seed 1)
     ws <- V.fromList <$> dictAmEn
     V.toList <$> R.uniformShuffle ws g
randomStrs =
  do g <- R.initialize (seed 3)
     revReplicateM numRandomStr $ do
       n <- R.genFromTable distN g
       revReplicateM (n+1) (uniformAlphabet g)
  where
    distN = R.tableBinomial 12 0.33
    alphabet = V.fromList ['a' .. 'z']
    numAlphabet = V.length alphabet
    uniformAlphabet g = (alphabet V.!) <$> R.uniformR (0, numAlphabet-1) g

revReplicateM :: (Monad m) => Int -> m a -> m [a]
revReplicateM n ma = loop n []
  where
    loop 0 acc = return acc
    loop i acc = ma >>= \a -> loop (i-1) (a:acc)
