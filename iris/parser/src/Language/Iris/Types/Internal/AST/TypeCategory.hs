{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Iris.Types.Internal.AST.TypeCategory
  ( TypeCategory (..),
    type (<=!),
    type (<=?),
    LAZY,
    STRICT,
    OBJECT,
    ToAny (..),
    FromAny (..),
    IS_OBJECT,
  )
where

import Language.Iris.Types.Internal.AST.Base
  ( FALSE,
    TRUE,
  )
import Language.Iris.Types.Internal.AST.Stage (Stage)
import Relude

data TypeCategory
  = STRICT
  | LAZY
  | IS_OBJECT TypeCategory
  deriving (Show, Eq, Ord)

type STRICT = 'STRICT

type LAZY = 'LAZY

type IS_OBJECT = 'IS_OBJECT

type OBJECT = IS_OBJECT LAZY

class ToAny a where
  toAny :: a k (s :: Stage) -> a LAZY s

class FromAny a (k :: TypeCategory) where
  fromAny :: a LAZY (s :: Stage) -> Maybe (a k s)

type (a :: TypeCategory) <=! (b :: TypeCategory) = a <=? b ~ TRUE

-- <=
type family (elem :: TypeCategory) <=? (cat :: TypeCategory) :: Bool where
  'STRICT <=? 'LAZY = TRUE
  'IS_OBJECT 'STRICT <=? 'STRICT = TRUE
  'IS_OBJECT 'STRICT <=? 'LAZY = TRUE
  'IS_OBJECT 'LAZY <=? 'LAZY = TRUE
  a <=? a = TRUE
  a <=? b = FALSE
