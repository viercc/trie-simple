module Util where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

trisect :: (Ord k) => Map k a -> Map k b -> (Map k a, Map k (a,b), Map k b)
trisect ma mb = (Map.difference ma mb,
                 Map.intersectionWith (,) ma mb,
                 Map.difference mb ma)

groupStrs :: (Eq c) => [[c]] -> (Bool, [(c,[[c]])])
groupStrs = foldr step (False, [])
  where
    step [] (_,es)     = (True, es)
    step (c:cs) (a,es) = case es of
      ((d,dss) : rest)
        | c == d -> (a, (d, cs : dss) : rest)
      _ -> (a, (c, [cs]) : es)
