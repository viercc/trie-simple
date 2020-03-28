{-# LANGUAGE TypeApplications #-}
module Main (main) where

import           Control.Monad
import           Control.Applicative(liftA2)

import           Test.QuickCheck

import           Data.Trie.Map          as T
import           Data.Trie.Map.Internal as T
import           Data.Trie.Map.Gen

import qualified Data.Map.Lazy     as Map
import           Data.Maybe        (fromMaybe)

import           Data.Semigroup

import           Props

main :: IO ()
main = do
  quickCheck (propFunctor @(TMap U))
  quickCheck (propApplicativeLeftU @(TMap U))
  quickCheck (propApplicativeRightU @(TMap U))
  quickCheck (propApplicativeAssoc @(TMap U))
  quickCheck monadAssoc
  putStrLn "Applicative fail:"
  putStrLn $ "(x,y,z) = " ++ show applicativeAssocCounterexample
  let (x,y,z) = applicativeAssocCounterexample
  putStrLn $ "x <++> (y <++> z) = " ++ show (x <++> (y <++> z))
  putStrLn $ "(x <++> y) <++> z = " ++ show ((x <++> y) <++> z)
  putStrLn "    where <++> = liftA2 (++)"

monadAssoc :: TMap U (TMap U (TMap U Int)) -> Property
monadAssoc tttb = join (join tttb) === join (fmap join tttb)

applicativeAssocCounterexample :: (TMap Char String, TMap Char String, TMap Char String)
applicativeAssocCounterexample = (t,u,t)
  where t = fromList [ ("x", "A"), ("xx", "B")]
        u = fromList [ ("xxx", "A"), ("x", "B")]

(<++>) :: Applicative f => f [a] -> f [a] -> f [a]
(<++>) = liftA2 (++)

--------------------------------------------------------------------

instance (Ord c) => Applicative (TMap c) where
  pure = just
  (<*>) = liftA2_ ($)

liftA2_ :: (Ord c) => (a -> b -> r) -> TMap c a -> TMap c b -> TMap c r
liftA2_ f ta tb = getFirst <$> appendWith (\a b -> First (f a b)) ta tb

instance (Ord c) => Monad (TMap c) where
  return = just
  ta >>= k = fromMaybe T.empty $ flatMap_ k ta

flatMap_ :: (Ord c) => (a -> TMap c b) -> TMap c a -> Maybe (TMap c b)
flatMap_ k = go
  where
    go (TMap (Node Nothing e)) =
      let e' = Map.mapMaybe go e
      in if Map.null e' then Nothing else Just (TMap (Node Nothing e'))
    go (TMap (Node (Just a) e)) =
      let TMap (Node mb e') = k a
          e'' = Map.mapMaybe go e
          e''' = Map.unionWith union e' e''
          result = TMap (Node mb e''')
      in if T.null result then Nothing else Just result
