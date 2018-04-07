module Data.Trie.Set(
  -- * Types
  TSet(),
  -- * Queries
  member, notMember,
  beginWith,
  null, count, enumerate,
  -- * Construction
  empty, epsilon,
  string, strings,
  insert, delete,
  -- * Combine
  union, intersection, difference,
  append,
  -- * Other operations
  prefixes, suffixes, infixes,
  -- * Conversion
  fromList, toList,
  fromAscList, toAscList,
  fromSet, toSet,
  -- * Parsing
  toParser, toParser_
)
where

import Prelude hiding (null)
import Data.Trie.Set.Hidden
