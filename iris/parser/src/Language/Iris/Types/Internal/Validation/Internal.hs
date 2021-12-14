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
    askTypeMember,
    getOperationType,
    askObjectType,
    __askType,
  )
where

import Control.Monad.Except (MonadError (throwError))
import Language.Iris.Types.Internal.AST
  ( FromAny,
    GQLError,
    LAZY,
    OBJECT,
    Operation (..),
    STRICT,
    TRUE,
    Token,
    TypeName,
    TypeRef,
    UnionMember (..),
    VALID,
    fromAny,
    getOperationDataType,
    internal,
    msg,
    typeConName,
  )
import Language.Iris.Types.Internal.AST.Name (packVariantTypeName)
import Language.Iris.Types.Internal.AST.TypeSystem
import Language.Iris.Types.Internal.Validation.Validator
  ( Scope (currentTypeName),
    SelectionValidator,
    ValidatorContext (schema),
    asksScope,
  )
import Relude hiding (empty)

askType ::
  Constraints m c cat s ctx =>
  Typed cat s TypeRef ->
  m (TypeDefinition cat s)
askType = untyped (__askType . typeConName)

__askType ::
  Constraints m c cat s ctx => TypeName -> m (TypeDefinition cat s)
__askType name =
  asks schema
    >>= maybe (throwError (unknownType name)) pure
      . lookupDataType name
    >>= kindConstraint

askTypeMember ::
  Constraints m c cat s ctx =>
  UnionMember cat s ->
  m (UnionMember cat s)
askTypeMember UnionMember {memberName, memberFields = Just fields, ..} = do
  typename <- asksScope currentTypeName
  pure
    UnionMember
      { memberName = packVariantTypeName typename memberName,
        memberFields = Just fields,
        ..
      }
askTypeMember UnionMember {memberName} = __askType memberName >>= constraintObject

askObjectType :: Constraints m c cat s ctx => TypeName -> m (UnionMember cat s)
askObjectType = __askType >=> constraintObject

type Constraints m c cat s ctx =
  ( MonadError GQLError m,
    Monad m,
    MonadReader (ValidatorContext s ctx) m,
    KindErrors cat,
    FromAny (TypeContent TRUE) cat
  )

getOperationType :: Operation a -> SelectionValidator (TypeDefinition OBJECT VALID)
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
  TypeDefinition LAZY s ->
  f (TypeDefinition k s)
_kindConstraint err anyType =
  maybe
    (throwError $ violation err (typeName anyType))
    pure
    (fromAny anyType)

class KindErrors c where
  kindConstraint :: KindConstraint f c => TypeDefinition LAZY s -> f (TypeDefinition c s)
  constraintObject :: MonadError GQLError m => TypeDefinition c s -> m (UnionMember c s)

instance KindErrors STRICT where
  kindConstraint = _kindConstraint " data type"
  constraintObject
    TypeDefinition
      { typeName,
        typeContent = StrictTypeContent typeFields
      } =
      case toList typeFields of
        [UnionMember {..}] -> pure (UnionMember {..})
        _ -> throwError (violation "data object" typeName)
  constraintObject TypeDefinition {typeName} = throwError (violation "data object" typeName)

instance KindErrors LAZY where
  kindConstraint = _kindConstraint " output type"
  constraintObject TypeDefinition {typeName, typeDescription, typeContent = LazyTypeContent fields} =
    pure (UnionMember typeDescription typeName (Just fields))
  constraintObject TypeDefinition {typeName} = throwError (violation "object" typeName)

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
