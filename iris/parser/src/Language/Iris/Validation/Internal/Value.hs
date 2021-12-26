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
    VALID,
    ValidValue,
    Value (..),
    Variable (..),
    VariableContent (..),
    Variant (..),
    Variants,
    atPositions,
    isSubtype,
    lookupTypeVariant,
    msg,
  )
import Language.Iris.Types.Internal.AST.Name (__typename)
import Language.Iris.Types.Internal.Validation
  ( askType,
    selectKnown,
    selectWithDefaultValue,
  )
import Language.Iris.Types.Internal.Validation.Internal (askListType, resolveTypeMember)
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
      currentTypeWrappers
    } <-
    asksScope id
  prefix <- inputMessagePrefix
  throwError $
    ( prefix
        <> typeViolation
          currentTypeWrappers
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

validateValueByField ::
  ValidateWithDefault c schemaS s =>
  FieldDefinition DATA_TYPE schemaS ->
  Value s ->
  Validator schemaS (InputContext c) (Value VALID)
validateValueByField field =
  inField field
    . validateInputByTypeRef
      (fieldType field)

-- VALIDATION
validateInputByTypeRef ::
  ValidateWithDefault ctx schemaS valueS =>
  TypeRef ->
  Value valueS ->
  InputValidator schemaS ctx ValidValue
validateInputByTypeRef t (ResolvedVariable ref variable) = checkTypeCompatibility t ref variable
validateInputByTypeRef (TypeRef name params isRequired) v
  | v == Null = if isRequired then violation Nothing Null else pure Null
  | null params = do
    typeDef <- askType name
    withScope (setType typeDef (TypeRef name [] isRequired)) $ validateUnwrapped (typeContent typeDef) v
  | otherwise = validateSeries name params v

validateSeries :: ValidateWithDefault ctx s valueS => TypeName -> [TypeRef] -> Value valueS -> Validator s (InputContext ctx) (Value VALID)
validateSeries name [t] (List vs) =
  askListType name
    >> List <$> traverse (validateInputByTypeRef t) vs
validateSeries "Map" [keyType,valueType] (List values) = List <$> traverse validateTuple values
  where
    validateTuple (List [k,v]) =  List <$> traverse (uncurry validateInputByTypeRef) [(keyType,k),(valueType,v)]
    validateTuple v = violation Nothing v
validateSeries _ _ entryValue = violation Nothing entryValue



validateUnwrapped ::
  ValidateWithDefault ctx schemaS valueS =>
  TypeContent DATA_TYPE schemaS ->
  Value valueS ->
  InputValidator schemaS ctx ValidValue
validateUnwrapped (DataTypeContent variants) (Object conName fields) =
  case toList variants of
    [Variant {variantFields}] ->
      Object conName <$> validateInputObject variantFields fields
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
  case lookupTypeVariant (Just conName) inputUnion of
    Left _ -> violation (Just $ msg conName <> " is not possible union type") (Object (Just conName) rawFields)
    Right memberTypeRef -> do
      fields <- variantFields <$> resolveTypeMember memberTypeRef
      Object (Just conName) <$> validateInputObject fields rawFields
validateStrictUnionType _ (Object Nothing fields)
  | member __typename fields =
    throwError $
      "use data constructor! field \"__typename\" was used as data constructor,"
        <> " this is only allowed for JSON variables."
  | otherwise = throwError "data union member typename was not provided"
validateStrictUnionType _ value = violation Nothing value
