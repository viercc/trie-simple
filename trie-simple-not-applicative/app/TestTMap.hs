{-# LANGUAGE TypeApplications #-}
module Main (main) where

import           Control.Applicative(liftA2)
import           Data.Trie.Map
import           Data.Semigroup (First(..))

main :: IO ()
main = do
  putStrLn "Applicative fail:"
  putStrLn $ "(x,y,z) = " ++ show applicativeAssocCounterexample
  let (x,y,z) = applicativeAssocCounterexample
  putStrLn $ "x <++> (y <++> z) = " ++ show (x <++> (y <++> z))
  putStrLn $ "(x <++> y) <++> z = " ++ show ((x <++> y) <++> z)
  putStrLn "    where <++> = liftA2 (++)"

applicativeAssocCounterexample :: (TMap Char String, TMap Char String, TMap Char String)
applicativeAssocCounterexample = (t,u,t)
  where t = fromList [ ("x", "A"), ("xx", "B")]
        u = fromList [ ("", "A"), ("xx", "B")]

(<++>) :: Applicative f => f [a] -> f [a] -> f [a]
(<++>) = liftA2 (++)

--------------------------------------------------------------------

instance (Ord c) => Applicative (TMap c) where
  pure = just
  liftA2 = liftA2_

liftA2_ :: (Ord c) => (a -> b -> r) -> TMap c a -> TMap c b -> TMap c r
liftA2_ f ta tb = getFirst <$> appendWith (\a b -> First (f a b)) ta tb
