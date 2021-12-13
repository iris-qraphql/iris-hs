{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Mergeable.Utils.KeyOf
  ( KeyOf (..),
    toPair,
  )
where

import Data.Mergeable
  ( Indexed (..),
  )
import Relude

class (Eq k, Hashable k) => KeyOf k a | a -> k where
  keyOf :: a -> k

instance (Eq k, Hashable k) => KeyOf k (k, a) where
  keyOf = fst

-- instance (Eq name, Hashable name) => KeyOf name (Ref name) where
--   keyOf = refName

instance (Eq k, Hashable k) => KeyOf k (Indexed k a) where
  keyOf = indexedKey

toPair :: KeyOf k a => a -> (k, a)
toPair x = (keyOf x, x)
