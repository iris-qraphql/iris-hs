{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Iris.Types.Internal.AST.Role
  ( Role (..),
    RESOLVER_TYPE,
    DATA_TYPE,
    ToAny (..),
    FromAny (..),
  )
where

import Language.Iris.Types.Internal.AST.Stage (Stage)
import Relude

data Role = DATA_TYPE | RESOLVER_TYPE deriving (Show, Eq, Ord)

type DATA_TYPE = 'DATA_TYPE

type RESOLVER_TYPE = 'RESOLVER_TYPE

class ToAny a where
  toAny :: a k (s :: Stage) -> a RESOLVER_TYPE s

class FromAny a (k :: Role) where
  fromAny :: a RESOLVER_TYPE (s :: Stage) -> Maybe (a k s)
