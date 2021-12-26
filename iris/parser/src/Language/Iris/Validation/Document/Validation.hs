{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Iris.Validation.Document.Validation
  ( ValidateSchema (..),
  )
where

import Language.Iris.Schema.Schema
  ( internalSchema,
  )
import Language.Iris.Types.Internal.AST
  ( (<:>),
    ArgumentDefinition (..),
    CONST,
    DATA_TYPE,
    DirectiveDefinition (..),
    DirectiveLocation (..),
    FieldDefinition (..),
    FieldName,
    GQLResult,
    RESOLVER_TYPE,
    Role,
    Schema (..),
    TypeContent (..),
    TypeDefinition (..),
    TypeRef (..),
    VALID,
    Value,
    Variant (..),
    toLocation
  )
import Language.Iris.Types.Internal.Config (Config (..))
import Language.Iris.Types.Internal.Validation
  ( InputSource (..),
    ValidatorContext (localContext),
    startInput,
  )
import Language.Iris.Types.Internal.Validation.Internal (Constraints, KindErrors, askListType, askType)
import Language.Iris.Types.Internal.Validation.SchemaValidator
  ( Field (..),
    ON_TYPE,
    SchemaValidator,
    TypeEntity (..),
    TypeSystemContext (..),
    inField,
    inType,
    runSchemaValidator,
  )
import Language.Iris.Validation.Document.TypeGuard (validateTypeGuard)
import Language.Iris.Validation.Internal.Directive
  ( validateDirectives,
  )
import Language.Iris.Validation.Internal.Value
  ( validateInputByTypeRef,
  )
import Relude hiding (empty, local)

class ValidateSchema s where
  validateSchema :: Bool -> Config -> Schema s -> GQLResult (Schema VALID)

instance ValidateSchema CONST where
  validateSchema withSystem config schema = do
    sysSchema <-
      if withSystem
        then internalSchema <:> schema
        else pure schema
    runSchemaValidator (typeCheck schema) config sysSchema

instance ValidateSchema VALID where
  validateSchema _ _ = pure

class TypeCheck a where
  type TypeContext a :: Type
  type TypeContext a = ()
  typeCheck :: a CONST -> SchemaValidator (TypeContext a) (a VALID)

instance TypeCheck Schema where
  typeCheck
    Schema
      { types,
        query,
        mutation,
        subscription,
        lists,
        directiveDefinitions
      } =
      Schema
        <$> traverse typeCheck types
        <*> typeCheck query
        <*> traverse typeCheck mutation
        <*> traverse typeCheck subscription
        <*> pure lists
        <*> traverse typeCheck directiveDefinitions

instance TypeCheck (TypeDefinition cat) where
  typeCheck
    TypeDefinition
      { typeName,
        typeDescription,
        typeDirectives,
        typeContent
      } =
      inType typeName $
        TypeDefinition
          typeDescription
          typeName
          <$> validateDirectives (toLocation typeContent) typeDirectives
          <*> typeCheck typeContent

instance TypeCheck (TypeContent cat) where
  type TypeContext (TypeContent cat) = TypeEntity ON_TYPE
  typeCheck ScalarTypeContent {..} = pure ScalarTypeContent {..}
  typeCheck DataTypeContent {dataVariants} =
    DataTypeContent <$> traverse typeCheck dataVariants
  typeCheck
    ResolverTypeContent
      { resolverTypeGuard,
        resolverVariants
      } =
      ResolverTypeContent
        <$> traverse (validateTypeGuard (toList resolverVariants)) resolverTypeGuard
        <*> traverse typeCheck resolverVariants

instance (FieldDirectiveLocation cat, KindErrors cat) => TypeCheck (FieldDefinition cat) where
  type TypeContext (FieldDefinition cat) = TypeEntity ON_TYPE
  typeCheck FieldDefinition {..} =
    inField
      fieldName
      ( FieldDefinition
          fieldDescription
          fieldName
          <$> traverse (traverse typeCheck) fieldArgs
          <*> validateFieldType (Proxy @cat) fieldType
          <*> validateDirectives (directiveLocation (Proxy @cat)) fieldDirectives
      )

class FieldDirectiveLocation (cat :: Role) where
  directiveLocation :: Proxy cat -> DirectiveLocation

instance FieldDirectiveLocation RESOLVER_TYPE where
  directiveLocation _ = FIELD_DEFINITION

instance FieldDirectiveLocation DATA_TYPE where
  directiveLocation _ = DATA_FIELD_DEFINITION

instance TypeCheck DirectiveDefinition where
  typeCheck DirectiveDefinition {directiveDefinitionArgs = arguments, ..} =
    inType "Directive"
      $ inField directiveDefinitionName
      $ do
        directiveDefinitionArgs <- traverse typeCheck arguments
        pure DirectiveDefinition {..}

instance TypeCheck ArgumentDefinition where
  type TypeContext ArgumentDefinition = Field ON_TYPE
  typeCheck ArgumentDefinition {..} =
    ArgumentDefinition
      argDescription
      argName
      <$> validateFieldType (Proxy @DATA_TYPE) argType
      <*> traverse (validateDefaultValue argType (Just argName)) argDefaultValue
      <*> validateDirectives ARGUMENT_DEFINITION argDirectives

validateFieldType ::
  forall m c cat s ctx.
  Constraints m c cat s ctx =>
  Proxy cat ->
  TypeRef ->
  m TypeRef
validateFieldType _ = validateTypRef 
  where
    validateTypRef :: TypeRef -> m TypeRef
    validateTypRef TypeRef {typeRefName, typeParameters = [], ..} = do
      (_ :: TypeDefinition cat s) <- askType typeRefName
      pure TypeRef {typeParameters = [], ..}
    validateTypRef TypeRef {..} =
      traverse_ validateTypRef typeParameters
        *> askListType typeRefName
        $> TypeRef {..}

validateDefaultValue ::
  TypeRef ->
  Maybe FieldName ->
  Value CONST ->
  SchemaValidator (Field ON_TYPE) (Value VALID)
validateDefaultValue typeRef argName value = do
  Field fName _ (TypeEntity _ typeName) <- asks (local . localContext)
  startInput (SourceInputField typeName fName argName) (validateInputByTypeRef typeRef value)

instance (KindErrors cat, FieldDirectiveLocation cat) => TypeCheck (Variant cat) where
  type TypeContext (Variant cat) = TypeEntity ON_TYPE
  typeCheck Variant {..} =
    Variant variantDescription variantName membership
      <$> traverse typeCheck variantFields
