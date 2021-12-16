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
  ( ArgumentDefinition (..),
    CONST,
    DirectiveDefinition (..),
    DirectiveLocation (..),
    FieldContent (..),
    FieldDefinition (..),
    FieldName,
    GQLResult,
    RESOLVER_TYPE,
    DATA_TYPE,
    Schema (..),
    Role,
    TypeContent (..),
    TypeDefinition (..),
    TypeRef (..),
    Typed (..),
    UnionMember (..),
    VALID,
    Value,
    kindOf,
    (<:>),
  )
import Language.Iris.Types.Internal.Config (Config (..))
import Language.Iris.Types.Internal.Validation
  ( InputSource (..),
    ValidatorContext (localContext),
    startInput,
  )
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

----- TypeCheck -------------------------------
---
---
---
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
        directiveDefinitions
      } =
      Schema
        <$> traverse typeCheck types
        <*> typeCheck query
        <*> traverse typeCheck mutation
        <*> traverse typeCheck subscription
        <*> traverse typeCheck directiveDefinitions

instance TypeCheck (TypeDefinition cat) where
  typeCheck
    t@TypeDefinition
      { typeName,
        typeDescription,
        typeDirectives,
        typeContent
      } =
      inType typeName $
        TypeDefinition
          typeDescription
          typeName
          <$> validateDirectives (TYPE_DIRECTIVE $ kindOf t) typeDirectives
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

instance FieldDirectiveLocation cat => TypeCheck (FieldDefinition cat) where
  type TypeContext (FieldDefinition cat) = TypeEntity ON_TYPE
  typeCheck FieldDefinition {..} =
    inField
      fieldName
      ( FieldDefinition
          fieldDescription
          fieldName
          <$> traverse checkFieldContent fieldContent
          <*> pure fieldType -- TODO: check if type exists
          <*> validateDirectives (directiveLocation (Proxy @cat)) fieldDirectives
      )
    where
      checkFieldContent :: FieldContent cat CONST -> SchemaValidator (Field ON_TYPE) (FieldContent cat VALID)
      checkFieldContent (ResolverFieldContent args) = ResolverFieldContent <$> traverse typeCheck args
      checkFieldContent DataFieldContent = pure DataFieldContent

class FieldDirectiveLocation (cat :: Role) where
  directiveLocation :: Proxy cat -> DirectiveLocation

instance FieldDirectiveLocation RESOLVER_TYPE where
  directiveLocation _ = FIELD_DEFINITION

instance FieldDirectiveLocation DATA_TYPE where
  directiveLocation _ = DATA_FIELD_DEFINITION

instance TypeCheck DirectiveDefinition where
  typeCheck DirectiveDefinition {directiveDefinitionArgs = arguments, ..} =
    inType "Directive" $
      inField directiveDefinitionName $
        do
          directiveDefinitionArgs <- traverse typeCheck arguments
          pure DirectiveDefinition {..}

instance TypeCheck ArgumentDefinition where
  type TypeContext ArgumentDefinition = Field ON_TYPE
  typeCheck (ArgumentDefinition FieldDefinition {..} defaultValue) =
    ArgumentDefinition
      <$> ( FieldDefinition
              fieldDescription
              fieldName
              Nothing
              fieldType
              <$> validateDirectives ARGUMENT_DEFINITION fieldDirectives
          )
        <*> traverse (validateDefaultValue fieldType (Just fieldName)) defaultValue

validateDefaultValue ::
  TypeRef ->
  Maybe FieldName ->
  Value CONST ->
  SchemaValidator (Field ON_TYPE) (Value VALID)
validateDefaultValue typeRef argName value = do
  Field fName _ (TypeEntity _ typeName) <- asks (local . localContext)
  startInput (SourceInputField typeName fName argName) (validateInputByTypeRef (Typed typeRef) value)

instance FieldDirectiveLocation cat => TypeCheck (UnionMember cat) where
  type TypeContext (UnionMember cat) = TypeEntity ON_TYPE
  typeCheck UnionMember {..} =
    UnionMember memberDescription memberName membership
      <$> traverse typeCheck memberFields
