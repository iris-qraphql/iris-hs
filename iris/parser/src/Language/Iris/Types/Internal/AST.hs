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
    ScalarDefinition (..),
    FieldsDefinition,
    ArgumentDefinition (..),
    Variants,
    ArgumentsDefinition,
    FieldDefinition (..),
    TypeContent (..),
    TypeDefinition (..),
    Schema (..),
    TypeRef (..),
    OperationType (..),
    QUERY,
    MUTATION,
    SUBSCRIPTION,
    Directive (..),
    Role (..),
    VariableContent (..),
    TypeDefinitions,
    toLocation,
    Subtyping (..),
    isNotSystemTypeName,
    fieldVisibility,
    lookupDeprecation,
    ExecutableDocument (..),
    Variables,
    OrdMap (..),
    GQLError (..),
    GQLErrors,
    ObjectEntry (..),
    UnionTag (..),
    DATA_TYPE,
    RESOLVER_TYPE,
    TypeName,
    Msg (..),
    intercalate,
    Directives,
    DirectivesDefinition,
    DirectiveDefinition (..),
    DirectiveLocation (..),
    FieldContent (..),
    fieldArguments,
    Variant (..),
    RawTypeDefinition (..),
    UnionSelection,
    mkSchema,
    getOperationDataType,
    ToRESOLVER (..),
    ToDATA (..),
    unitTypeName,
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
    lookupTypeVariant,
    getVariantName,
    variantTypeName,
    ListDefinition (..),
  )
where

import Data.Mergeable.OrdMap (OrdMap (..))
import Data.Mergeable.SafeHashMap (SafeHashMap)
import Data.Mergeable.Utils (Result)
import Language.Haskell.TH.Syntax (Lift)
import Language.Iris.Types.Internal.AST.Base
import Language.Iris.Types.Internal.AST.Directive (DirectiveLocation (..))
import Language.Iris.Types.Internal.AST.Error
import Language.Iris.Types.Internal.AST.Fields
import Language.Iris.Types.Internal.AST.Name
import Language.Iris.Types.Internal.AST.OperationType
import Language.Iris.Types.Internal.AST.Role
import Language.Iris.Types.Internal.AST.Schema
import Language.Iris.Types.Internal.AST.Selection
import Language.Iris.Types.Internal.AST.Stage
import Language.Iris.Types.Internal.AST.Type
import Language.Iris.Types.Internal.AST.TypeSystem
import Language.Iris.Types.Internal.AST.Value
import Language.Iris.Types.Internal.AST.Variant
import Prelude (Show)

type Variables = SafeHashMap FieldName ResolvedValue

data ExecutableDocument = ExecutableDocument
  { inputVariables :: Variables,
    operation :: Operation RAW,
    fragments :: Fragments RAW
  }
  deriving (Show, Lift)

type GQLResult = Result GQLError
