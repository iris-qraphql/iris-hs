{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Iris.Types.Internal.Validation.Internal
  ( askType,
    resolveTypeMember,
    getOperationType,
    askObjectType,
    askListType,
    Constraints,
  )
where

import Control.Monad.Except (MonadError (throwError))
import Data.Mergeable.IsMap (selectBy)
import Language.Iris.Types.Internal.AST
  ( DATA_TYPE,
    GQLError,
    ListDefinition (ListDefinition),
    Operation (..),
    RESOLVER_TYPE,
    Role,
    ToDATA,
    ToRESOLVER (toRESOLVER),
    TypeContent (..),
    TypeDefinition (..),
    TypeName,
    VALID,
    getOperationDataType,
    internal,
    lists,
    lookupDataType,
    msg,
    toDATA,
  )
import Language.Iris.Types.Internal.AST.Name (packVariantTypeName, unpackVariantTypeName)
import Language.Iris.Types.Internal.AST.Variant
import Language.Iris.Types.Internal.Validation.Validator
  ( SelectionValidator,
    ValidatorContext (schema),
  )
import Relude hiding (empty)

askListType ::
  ( MonadReader (ValidatorContext s ctx) m,
    MonadError GQLError m
  ) =>
  TypeName ->
  m ListDefinition
askListType "List" = pure $ ListDefinition Nothing "List"
askListType name =
  asks (lists . schema)
    >>= selectBy ("Unknown list " <> msg name <> ".") name

askType :: Constraints m c cat s ctx => TypeName -> m (TypeDefinition cat s)
askType name = asks schema >>= lookupDataType name >>= kindConstraint

askObjectType :: Constraints m c cat s ctx => TypeName -> m (Variant cat s)
askObjectType name = case unpackVariantTypeName name of
  (tName, variantName) -> askType tName >>= constraintObject variantName

resolveTypeMember ::
  Constraints m c cat s ctx =>
  Variant cat s ->
  m (Variant cat s)
resolveTypeMember Variant {variantName, membership = Just name, variantFields, ..} =
  pure
    Variant
      { variantName = packVariantTypeName name variantName,
        membership = Just name,
        ..
      }
resolveTypeMember Variant {variantName} = askType variantName >>= constraintObject Nothing

type Constraints m c (cat :: Role) s ctx =
  ( MonadError GQLError m,
    Monad m,
    MonadReader (ValidatorContext s ctx) m,
    KindErrors cat,
    ToDATA TypeContent
  )

getOperationType :: Operation a -> SelectionValidator (Variant RESOLVER_TYPE VALID)
getOperationType operation = asks schema >>= getOperationDataType operation

type KindConstraint f = (MonadError GQLError f, ToDATA TypeDefinition)

class KindErrors c where
  kindConstraint :: KindConstraint f => TypeDefinition RESOLVER_TYPE s -> f (TypeDefinition c s)
  constraintObject :: MonadError GQLError m => Maybe TypeName -> TypeDefinition c s -> m (Variant c s)

instance KindErrors DATA_TYPE where
  kindConstraint = toDATA
  constraintObject
    _
    TypeDefinition {typeContent = DataTypeContent (Variant {..} :| [])} = pure (Variant {..})
  constraintObject _ TypeDefinition {typeName} = throwError (violation "data object" typeName)

instance KindErrors RESOLVER_TYPE where
  kindConstraint = pure . toRESOLVER
  constraintObject Nothing TypeDefinition {typeContent = ResolverTypeContent _ (member :| [])} = pure member
  constraintObject (Just variantName) TypeDefinition {typeContent = ResolverTypeContent _ variants} =
    lookupTypeVariant (Just variantName) variants
  constraintObject _ TypeDefinition {typeName} = throwError (violation "object" typeName)

violation ::
  Text ->
  TypeName ->
  GQLError
violation kind typeName =
  internal $
    "Type \"" <> msg typeName
      <> "\" must be an"
      <> msg kind
      <> "."
