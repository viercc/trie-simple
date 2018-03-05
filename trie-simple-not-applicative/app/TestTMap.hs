module Main (main) where

import           Control.Monad
import           Control.Applicative(liftA2)

import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

import           Data.Trie.Map     as T
import           Data.Trie.Map.Gen

import qualified Data.Map.Lazy     as Map
import           Data.Maybe        (fromMaybe)

main :: IO ()
main = do
  quickBatch (functor typeTag3)
  quickBatch (applicative typeTag3)
  quickCheck monadAssoc
  putStrLn "Applicative fail:"
  putStrLn $ "(x,y,z) = " ++ show applicativeAssocCounterexample
  let (x,y,z) = applicativeAssocCounterexample
  putStrLn $ "x <++> (y <++> z) = " ++ show (x <++> (y <++> z))
  putStrLn $ "(x <++> y) <++> z = " ++ show ((x <++> y) <++> z)
  putStrLn "    where <++> = liftA2 (++)"

typeTag3 :: TMap U (Bool, Bool, Bool)
typeTag3 = empty

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
  liftA2 = product_

product_ :: (Ord c) => (x -> y -> z) -> TMap c x -> TMap c y -> TMap c z
product_ f x y =
  if T.null y
    then empty
    else go x
  where
    go (TMap (Node Nothing e)) =
      let e' = Map.map go e
      in TMap (Node Nothing e')
    go (TMap (Node (Just ax) e)) =
      let TMap (Node maz e') = fmap (f ax) y
          e'' = Map.map go e
          e''' = Map.unionWith union e' e''
      in TMap (Node maz e''')

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
      do let tb@(TMap (Node mb e')) = k a
         guard (not (T.null tb))
         let e'' = Map.mapMaybe go e
             e''' = Map.unionWith union e' e''
             result = TMap (Node mb e''')
         guard (not (T.null result))
         return result
