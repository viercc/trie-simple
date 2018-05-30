module Data.Trie.Map(
  -- * Types
  TMap(),
  -- * Queries
  match,
  lookup,
  member, notMember,
  null, count,
  keys, elems,
  -- * Construction
  empty, just,
  singleton,

  -- * Single item modification
  insertWith, insert,
  deleteWith, delete,

  adjust, revise, update, alter,

  -- * Combine
  union, unionWith,
  intersection, intersectionWith,
  difference, differenceWith,
  appendWith,

  -- * Conversion
  toList, fromList,
  toAscList, fromAscList,
  toMap, fromMap,
  keysTSet, fromTSet,

  -- * Parsing
  toParser, toParser_, toParser__,

  -- * Traversing with keys
  traverseWithKey, mapWithKey, foldMapWithKey, foldrWithKey,
)
where

import           Prelude              hiding (lookup, null)

import           Data.Trie.Map.Hidden
