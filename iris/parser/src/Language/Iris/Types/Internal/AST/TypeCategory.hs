{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Iris.Types.Internal.AST.TypeCategory
  ( TypeCategory (..),
    LAZY,
    STRICT,
    ToAny (..),
    FromAny (..),
  )
where


import Language.Iris.Types.Internal.AST.Stage (Stage)
import Relude

data TypeCategory = STRICT | LAZY deriving (Show, Eq, Ord)

type STRICT = 'STRICT

type LAZY = 'LAZY

class ToAny a where
  toAny :: a k (s :: Stage) -> a LAZY s

class FromAny a (k :: TypeCategory) where
  fromAny :: a LAZY (s :: Stage) -> Maybe (a k s)
