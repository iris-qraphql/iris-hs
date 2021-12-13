{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Iris.Error.Resolver
  ( TypeGuardError (..),
    partialImplements,
  )
where

import Language.Iris.Types.Internal.AST.Error
  ( GQLError,
    msg,
  )
import Language.Iris.Types.Internal.AST.Type (TypeRef)
import Language.Iris.Types.Internal.Validation.SchemaValidator
  ( Field (..),
    InterfaceName (..),
    ON_INTERFACE,
    TypeEntity (..),
    renderField,
  )
import Relude

data TypeGuardError
  = UnexpectedType
      { expectedType :: TypeRef,
        foundType :: TypeRef
      }
  | Missing

partialImplements :: Field ON_INTERFACE -> TypeGuardError -> GQLError
partialImplements (Field fieldName argName (TypeEntity (OnTypeGuard guardName memberName) typename)) errorType =
  "type guard field " <> maybe "" (const "argument ") argName
    <> renderField (typename <> "." <> guardName) fieldName argName
    <> detailedMessageGen
      (renderField memberName fieldName argName)
      (maybe (msg memberName) (const $ renderField memberName fieldName Nothing) argName)
      errorType

detailedMessageGen :: GQLError -> GQLError -> TypeGuardError -> GQLError
detailedMessageGen pl1 _ UnexpectedType {expectedType, foundType} =
  " expects type "
    <> msg expectedType
    <> " but "
    <> pl1
    <> " is type "
    <> msg foundType
    <> "."
detailedMessageGen _ pl2 Missing = " expected but " <> pl2 <> " does not provide it."
