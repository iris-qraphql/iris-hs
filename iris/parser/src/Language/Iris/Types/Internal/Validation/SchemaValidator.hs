{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Iris.Types.Internal.Validation.SchemaValidator
  ( SchemaValidator,
    TypeSystemContext (..),
    renderField,
    withLocalContext,
    runSchemaValidator,
    inTypeGuard,
    inType,
    inField,
    inArgument,
    ON_INTERFACE,
    ON_TYPE,
    TypeEntity (..),
    Field (..),
    InterfaceName (..),
    PLACE,
  )
where

import Language.Iris.Types.Internal.AST
  ( CONST,
    FieldName,
    GQLResult,
    Name (unpackName),
    TypeName,
    mkBaseType, DirectiveLocation (RESOLVER),
  )
import Language.Iris.Types.Internal.AST.Schema (Schema)
import Language.Iris.Types.Internal.Config (Config)
import Language.Iris.Types.Internal.Validation
  ( Scope (..),
    ScopeKind (TYPE),
    runValidator,
  )
import Language.Iris.Types.Internal.Validation.Validator
  ( Validator (..),
    renderField,
    withContext,
    withScope,
  )
import Relude hiding (local)

inTypeGuard ::
  TypeName ->
  TypeName ->
  SchemaValidator (TypeEntity 'ON_INTERFACE) v ->
  SchemaValidator (TypeEntity 'ON_TYPE) v
inTypeGuard guardName name =
  pushPath (unpackName guardName)
    . withLocalContext (\t -> t {interfaceName = OnTypeGuard guardName name})

inType ::
  TypeName ->
  SchemaValidator (TypeEntity 'ON_TYPE) v ->
  SchemaValidator () v
inType name = pushPath (unpackName name) . withLocalContext (const (TypeEntity OnType name))

inField ::
  FieldName ->
  SchemaValidator (Field p) v ->
  SchemaValidator (TypeEntity p) v
inField name = pushPath (unpackName name) . withLocalContext (Field name Nothing)

inArgument ::
  FieldName ->
  SchemaValidator (Field p) v ->
  SchemaValidator (Field p) v
inArgument name = pushPath (unpackName name) . withLocalContext (\field -> field {fieldArgument = Just name})

data PLACE = ON_INTERFACE | ON_TYPE

type ON_INTERFACE = 'ON_INTERFACE

type ON_TYPE = 'ON_TYPE

data InterfaceName (p :: PLACE) where
  OnTypeGuard :: TypeName -> TypeName -> InterfaceName 'ON_INTERFACE
  OnType :: InterfaceName 'ON_TYPE

data TypeEntity (p :: PLACE) = TypeEntity
  { interfaceName :: InterfaceName p,
    typeName :: TypeName
  }

data Field p = Field
  { fieldName :: FieldName,
    fieldArgument :: Maybe FieldName,
    fieldOf :: TypeEntity p
  }

initialScope :: Scope
initialScope =
  Scope
    { position = Nothing,
      currentTypeName = "Root",
      currentTypeKind = RESOLVER,
      currentTypeWrappers = mkBaseType,
      kind = TYPE,
      fieldName = "Root",
      path = []
    }

newtype TypeSystemContext c = TypeSystemContext
  {local :: c}
  deriving (Show)

pushPath :: Text -> SchemaValidator a v -> SchemaValidator a v
pushPath name = withScope (\x -> x {path = path x <> [name]})

withLocalContext :: (a -> b) -> SchemaValidator b v -> SchemaValidator a v
withLocalContext = withContext . updateLocal

updateLocal :: (a -> b) -> TypeSystemContext a -> TypeSystemContext b
updateLocal f ctx = ctx {local = f (local ctx)}

type SchemaValidator c = Validator CONST (TypeSystemContext c)

runSchemaValidator :: Validator s (TypeSystemContext ()) a -> Config -> Schema s -> GQLResult a
runSchemaValidator value config sysSchema =
  runValidator
    value
    config
    sysSchema
    initialScope
    TypeSystemContext
      { local = ()
      }
