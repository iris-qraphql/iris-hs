{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Iris.Error.Data
  ( typeViolation,
  )
where

import Language.Iris.Types.Internal.AST
  ( GQLError,
    TypeRef (..),
    Value,
    msg,
  )
import Data.Semigroup ((<>))

typeViolation :: TypeRef -> Value s -> GQLError
typeViolation expected found =
  "Expected type "
    <> msg expected
    <> " found "
    <> msg found
    <> "."

