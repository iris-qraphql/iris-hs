{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Iris.Types.Internal.AST.Role
  ( Role (..),
    LAZY,
    STRICT,
    ToAny (..),
    FromAny (..),
  )
where

import Language.Iris.Types.Internal.AST.Stage (Stage)
import Relude

data Role = STRICT | LAZY deriving (Show, Eq, Ord)

type STRICT = 'STRICT

type LAZY = 'LAZY

class ToAny a where
  toAny :: a k (s :: Stage) -> a LAZY s

class FromAny a (k :: Role) where
  fromAny :: a LAZY (s :: Stage) -> Maybe (a k s)
