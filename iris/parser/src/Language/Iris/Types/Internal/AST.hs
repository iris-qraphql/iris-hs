{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Iris.Types.Internal.AST
  ( Ref (..),
    Position (..),
    Message,
    FieldName,
    Description,
    Stage,
    CONST,
    VALID,
    RAW,
    Value (..),
    ScalarValue (..),
    Object,
    replaceValue,
    decodeScientific,
    RawValue,
    ValidValue,
    RawObject,
    ValidObject,
    ResolvedObject,
    ResolvedValue,
    Argument (..),
    Arguments,
    SelectionSet,
    SelectionContent (..),
    Selection (..),
    Fragments,
    Fragment (..),
    Operation (..),
    Variable (..),
    VariableDefinitions,
    DefaultValue,
    getOperationName,
    ScalarDefinition (..),
    StrictUnionContent,
    FieldsDefinition,
    ArgumentDefinition (..),
    UnionTypeDefinition,
    ArgumentsDefinition,
    FieldDefinition (..),
    InputFieldsDefinition,
    TypeContent (..),
    TypeDefinition (..),
    Schema (..),
    TypeKind (..),
    TypeWrapper (..),
    TypeRef (..),
    OperationType (..),
    QUERY,
    MUTATION,
    SUBSCRIPTION,
    Directive (..),
    Role (..),
    VariableContent (..),
    TypeDefinitions,
    initTypeLib,
    kindOf,
    toNullable,
    isNullable,
    Subtyping (..),
    isNotSystemTypeName,
    isLeaf,
    isResolverType,
    createScalarType,
    mkTypeRef,
    fieldVisibility,
    lookupDeprecated,
    lookupDeprecatedReason,
    lookupWith,
    ExecutableDocument (..),
    Variables,
    unsafeFromFields,
    OrdMap (..),
    GQLError (..),
    GQLErrors,
    ObjectEntry (..),
    UnionTag (..),
    STRICT,
    LAZY,
    TypeName,
    Token,
    Msg (..),
    intercalate,
    Directives,
    DirectivesDefinition,
    DirectiveDefinition (..),
    DirectiveLocation (..),
    FieldContent (..),
    fieldArguments,
    mkType,
    mkObjectField,
    UnionMember (..),
    RawTypeDefinition (..),
    RootOperationTypeDefinition (..),
    UnionSelection,
    SchemaDefinition (..),
    buildSchema,
    getOperationDataType,
    Typed (Typed),
    typed,
    untyped,
    ToAny (..),
    FromAny (..),
    mkField,
    defineSchemaWith,
    unitFieldName,
    unitTypeName,
    mkBaseType,
    mkMaybeType,
    packName,
    unpackName,
    at,
    atPositions,
    typeDefinitions,
    FragmentName,
    isInternal,
    internal,
    splitSystemSelection,
    lookupDataType,
    Name,
    withPath,
    __typename,
    GQLResult,
    startHistory,
    HistoryT,
    (<:>),
    mergeNonEmpty,
  )
where

import Data.Mergeable.OrdMap (OrdMap (..))
import Data.Mergeable.SafeHashMap (SafeHashMap)
import Data.Mergeable.Utils (Result)
import Language.Haskell.TH.Syntax (Lift)
import Language.Iris.Types.Internal.AST.Base
import Language.Iris.Types.Internal.AST.DirectiveLocation (DirectiveLocation (..))
import Language.Iris.Types.Internal.AST.Error
import Language.Iris.Types.Internal.AST.Fields
import Language.Iris.Types.Internal.AST.Name
import Language.Iris.Types.Internal.AST.OperationType
import Language.Iris.Types.Internal.AST.Selection
import Language.Iris.Types.Internal.AST.Stage
import Language.Iris.Types.Internal.AST.Type
import Language.Iris.Types.Internal.AST.Role
import Language.Iris.Types.Internal.AST.TypeSystem
import Language.Iris.Types.Internal.AST.Union
import Language.Iris.Types.Internal.AST.Value
import Prelude (Show)

type Variables = SafeHashMap FieldName ResolvedValue

data ExecutableDocument = ExecutableDocument
  { inputVariables :: Variables,
    operation :: Operation RAW,
    fragments :: Fragments RAW
  }
  deriving (Show, Lift)

type GQLResult = Result GQLError
