{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Iris.Validation.Internal.Value
  ( validateInputByTypeRef,
    validateInputByType,
    ValidateWithDefault,
  )
where

import Control.Monad.Except (throwError)
import Data.Mergeable.IsMap (member)
import Language.Iris.Error.Data (typeViolation)
import Language.Iris.Error.Variable (incompatibleVariableType)
import Language.Iris.Types.Internal.AST
  ( CONST,
    DATA_TYPE,
    FieldDefinition (..),
    FieldName,
    FieldsDefinition,
    GQLError,
    Object,
    ObjectEntry (..),
    Ref (..),
    ScalarDefinition (..),
    ScalarValue (..),
    TypeContent (..),
    TypeDefinition (..),
    TypeName,
    TypeRef (..),
    TypeWrapper (..),
    VALID,
    ValidValue,
    Value (..),
    Variable (..),
    VariableContent (..),
    Variant (..),
    Variants,
    atPositions,
    isNullable,
    isSubtype,
    msg,
  )
import Language.Iris.Types.Internal.AST.Name (__typename)
import Language.Iris.Types.Internal.Validation
  ( askType,
    selectKnown,
    selectWithDefaultValue,
  )
import Language.Iris.Types.Internal.Validation.Internal (lookupTypeVariant, resolveTypeMember)
import Language.Iris.Types.Internal.Validation.Scope (setType)
import Language.Iris.Types.Internal.Validation.Validator
import Relude hiding (empty)

violation ::
  Maybe GQLError ->
  Value s ->
  InputValidator schemaS ctx a
violation message value = do
  Scope
    { position,
      currentTypeName,
      currentTypeWrappers
    } <-
    asksScope id
  prefix <- inputMessagePrefix
  throwError $
    ( prefix
        <> typeViolation
          (TypeRef currentTypeName currentTypeWrappers)
          value
        <> maybe "" (" " <>) message
    )
      `atPositions` position

checkTypeCompatibility ::
  TypeRef ->
  Ref FieldName ->
  Variable VALID ->
  InputValidator schemaS ctx ValidValue
checkTypeCompatibility valueType ref var@Variable {variableValue = ValidVariableValue value, variableType}
  | variableType `isSubtype` valueType = pure value
  | otherwise = throwError $ incompatibleVariableType ref var valueType

validateInputByTypeRef ::
  ValidateWithDefault c schemaS s =>
  TypeRef ->
  Value s ->
  Validator schemaS (InputContext c) (Value VALID)
validateInputByTypeRef
  ref
  value = do
    inputTypeDef <- askType ref
    validateInputByType
      (typeWrappers ref)
      inputTypeDef
      value

validateValueByField ::
  ValidateWithDefault c schemaS s =>
  FieldDefinition DATA_TYPE schemaS ->
  Value s ->
  Validator schemaS (InputContext c) (Value VALID)
validateValueByField field =
  inField field
    . validateInputByTypeRef
      (fieldType field)

-- Validate data Values
validateInputByType ::
  ValidateWithDefault ctx schemaS valueS =>
  TypeWrapper ->
  TypeDefinition DATA_TYPE schemaS ->
  Value valueS ->
  InputValidator schemaS ctx ValidValue
validateInputByType tyWrappers typeDef =
  withScope (setType typeDef tyWrappers) . validateWrapped tyWrappers typeDef

-- VALIDATION
validateWrapped ::
  ValidateWithDefault ctx schemaS valueS =>
  TypeWrapper ->
  TypeDefinition DATA_TYPE schemaS ->
  Value valueS ->
  InputValidator schemaS ctx ValidValue
-- Validate Null. value = null ?
validateWrapped wrappers _ (ResolvedVariable ref variable) = do
  typeName <- asksScope currentTypeName
  checkTypeCompatibility (TypeRef typeName wrappers) ref variable
validateWrapped wrappers _ Null
  | isNullable wrappers = pure Null
  | otherwise = violation Nothing Null
-- Validate LIST
validateWrapped (TypeList wrappers _) tyCont (List list) =
  List <$> traverse (validateInputByType wrappers tyCont) list
{-- 2. VALIDATE TYPES, all wrappers are already Processed --}
validateWrapped BaseType {} TypeDefinition {typeContent} entryValue =
  validateUnwrapped typeContent entryValue
{-- 3. THROW ERROR: on invalid values --}
validateWrapped _ _ entryValue = violation Nothing entryValue

validateUnwrapped ::
  ValidateWithDefault ctx schemaS valueS =>
  TypeContent DATA_TYPE schemaS ->
  Value valueS ->
  InputValidator schemaS ctx ValidValue
validateUnwrapped (DataTypeContent variants) (Object conName fields) =
  case toList variants of
    [Variant {memberFields}] ->
      Object conName <$> validateInputObject memberFields fields
    _ -> validateStrictUnionType variants (Object conName fields)
validateUnwrapped (DataTypeContent tags) value =
  validateStrictUnionType tags value
validateUnwrapped (ScalarTypeContent dataScalar) value =
  validateScalar dataScalar value

validateInputObject ::
  ValidateWithDefault ctx schemaS valueS =>
  FieldsDefinition DATA_TYPE schemaS ->
  Object valueS ->
  InputValidator schemaS ctx (Object VALID)
validateInputObject fieldsDef object =
  traverse_ (`selectKnown` fieldsDef) object
    *> traverse (validateWithDefault object) fieldsDef

class ValidateWithDefault c schemaS s where
  validateWithDefault ::
    Object s ->
    FieldDefinition DATA_TYPE schemaS ->
    Validator schemaS (InputContext c) (ObjectEntry VALID)

instance ValidateWithDefault c VALID s where
  validateWithDefault object fieldDef@FieldDefinition {fieldName} =
    ObjectEntry fieldName
      <$> selectWithDefaultValue
        pure
        (validateValueByField fieldDef . entryValue)
        Nothing
        fieldDef
        object

instance ValidateWithDefault c CONST s where
  validateWithDefault object fieldDef@FieldDefinition {fieldName} =
    ObjectEntry fieldName
      <$> selectWithDefaultValue
        (validateValueByField fieldDef)
        (validateValueByField fieldDef . entryValue)
        Nothing
        fieldDef
        object

-- Leaf Validations
validateScalar ::
  ScalarDefinition ->
  Value s ->
  InputValidator schemaS ctx ValidValue
validateScalar ScalarDefinition {validateValue} value = do
  typeName <- asksScope currentTypeName
  scalarValue <- toScalar typeName value
  case validateValue scalarValue of
    Right _ -> pure scalarValue
    Left "" -> violation Nothing value
    Left message -> violation (Just $ msg message) value
  where
    toScalar :: TypeName -> Value s -> InputValidator schemaS ctx ValidValue
    toScalar typeName (Scalar x) | isValidDefault typeName x = pure (Scalar x)
    toScalar _ _ = violation Nothing value

isValidDefault :: TypeName -> ScalarValue -> Bool
isValidDefault "Boolean" = isBoolean
isValidDefault "String" = isString
isValidDefault "Float" = oneOf [isFloat, isInt]
isValidDefault "Int" = isInt
isValidDefault "ID" = oneOf [isInt, isFloat, isString]
isValidDefault _ = const True

oneOf :: [a -> Bool] -> a -> Bool
oneOf ls v = any (v &) ls

isBoolean :: ScalarValue -> Bool
isBoolean Boolean {} = True
isBoolean _ = False

isString :: ScalarValue -> Bool
isString String {} = True
isString _ = False

isFloat :: ScalarValue -> Bool
isFloat Float {} = True
isFloat _ = False

isInt :: ScalarValue -> Bool
isInt Int {} = True
isInt _ = False

validateStrictUnionType ::
  ValidateWithDefault ctx schemaS s =>
  Variants DATA_TYPE schemaS ->
  Value s ->
  InputValidator schemaS ctx ValidValue
validateStrictUnionType inputUnion (Object (Just conName) rawFields) =
  case lookupTypeVariant conName inputUnion of
    Left _ -> violation (Just $ msg conName <> " is not possible union type") (Object (Just conName) rawFields)
    Right memberTypeRef -> do
      fields <- memberFields <$> resolveTypeMember memberTypeRef
      Object (Just conName) <$> validateInputObject fields rawFields
validateStrictUnionType _ (Object Nothing fields)
  | member __typename fields =
    throwError $
      "use data constructor! field \"__typename\" was used as data constructor,"
        <> " this is only allowed for JSON variables."
  | otherwise = throwError "data union member typename was not provided"
validateStrictUnionType _ value = violation Nothing value
