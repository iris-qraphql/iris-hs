module Data.Mergeable
  ( Merge (..),
    mergeNoDuplicates,
    recursiveMerge,
    Indexed (..),
    NameCollision (..),
    ResolutionT,
    fromListT,
    indexed,
    resolveWith,
    runResolutionT,
    collect,
    IsMap (..),
    MergeMap,
    toNonEmpty,
    OrdMap,
    throwErrors,
    mergeConcat,
  )
where

import Data.Mergeable.Internal.Merge
import Data.Mergeable.Internal.NameCollision
import Data.Mergeable.Internal.Resolution
import Data.Mergeable.IsMap
import Data.Mergeable.MergeMap
import Data.Mergeable.OrdMap
