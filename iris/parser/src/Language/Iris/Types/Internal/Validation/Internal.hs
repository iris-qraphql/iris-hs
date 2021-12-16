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
    lookupTypeVariant,
  )
where

import Control.Monad.Except (MonadError (throwError))
import Language.Iris.Types.Internal.AST
  ( DATA_TYPE,
    FromAny,
    GQLError,
    Operation (..),
    RESOLVER_TYPE,
    Token,
    TypeName,
    TypeRef,
    VALID,
    Variant (..),
    fromAny,
    getOperationDataType,
    internal,
    msg,
    typeConName,
  )
import Language.Iris.Types.Internal.AST.Name (packVariantTypeName, unpackVariantTypeName)
import Language.Iris.Types.Internal.AST.TypeSystem
import Language.Iris.Types.Internal.Validation.Validator
  ( SelectionValidator,
    ValidatorContext (schema),
  )
import Relude hiding (empty)

askType ::
  Constraints m c cat s ctx =>
  Typed cat s TypeRef ->
  m (TypeDefinition cat s)
askType = untyped (__askType . typeConName)

noVariant :: MonadError GQLError m => TypeName -> m a
noVariant = throwError . violation "can't find variant"

__askType ::
  Constraints m c cat s ctx => TypeName -> m (TypeDefinition cat s)
__askType name =
  asks schema
    >>= maybe (throwError (unknownType name)) pure
      . lookupDataType name
    >>= kindConstraint

askObjectType :: Constraints m c cat s ctx => TypeName -> m (Variant cat s)
askObjectType name = case unpackVariantTypeName name of
  (tName, variantName) -> __askType tName >>= constraintObject variantName

resolveTypeMember ::
  Constraints m c cat s ctx =>
  Variant cat s ->
  m (Variant cat s)
resolveTypeMember Variant {variantName, membership = Just name, memberFields, ..} = do
  pure
    Variant
      { variantName = packVariantTypeName name variantName,
        membership = Just name,
        ..
      }
resolveTypeMember Variant {variantName} = __askType variantName >>= constraintObject Nothing

type Constraints m c (cat :: Role) s ctx =
  ( MonadError GQLError m,
    Monad m,
    MonadReader (ValidatorContext s ctx) m,
    KindErrors cat,
    FromAny TypeContent cat
  )

getOperationType :: Operation a -> SelectionValidator (Variant RESOLVER_TYPE VALID)
getOperationType operation = asks schema >>= getOperationDataType operation

unknownType :: TypeName -> GQLError
unknownType name = internal $ "Type \"" <> msg name <> "\" can't found in Schema."

type KindConstraint f c =
  ( MonadError GQLError f,
    FromAny TypeDefinition c
  )

_kindConstraint ::
  KindConstraint f k =>
  Token ->
  TypeDefinition RESOLVER_TYPE s ->
  f (TypeDefinition k s)
_kindConstraint err anyType =
  maybe
    (throwError $ violation err (typeName anyType))
    pure
    (fromAny anyType)

class KindErrors c where
  kindConstraint :: KindConstraint f c => TypeDefinition RESOLVER_TYPE s -> f (TypeDefinition c s)
  constraintObject :: MonadError GQLError m => Maybe TypeName -> TypeDefinition c s -> m (Variant c s)

instance KindErrors DATA_TYPE where
  kindConstraint = _kindConstraint " data type"
  constraintObject
    _
    TypeDefinition
      { typeName,
        typeContent = DataTypeContent typeFields
      } =
      case toList typeFields of
        [Variant {..}] -> pure (Variant {..})
        _ -> throwError (violation "data object" typeName)
  constraintObject _ TypeDefinition {typeName} = throwError (violation "data object" typeName)

lookupTypeVariant :: MonadError GQLError m => TypeName -> UnionTypeDefinition cat s -> m (Variant cat s)
lookupTypeVariant name variants = maybe (noVariant name) pure (find ((name ==) . variantName) variants)

instance KindErrors RESOLVER_TYPE where
  kindConstraint = _kindConstraint " output type"
  constraintObject Nothing TypeDefinition {typeContent = ResolverTypeContent _ (member :| [])} = pure member
  constraintObject (Just variantName) TypeDefinition {typeContent = ResolverTypeContent _ variants} =
    lookupTypeVariant variantName variants
  constraintObject _ TypeDefinition {typeName} = throwError (violation "object" typeName)

violation ::
  Token ->
  TypeName ->
  GQLError
violation kind typeName =
  internal $
    "Type \"" <> msg typeName
      <> "\" must be an"
      <> msg kind
      <> "."
