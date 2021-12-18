{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Iris.Types.Internal.AST.Fields
  ( Arguments,
    Argument (..),
    ArgumentDefinition (..),
    ArgumentsDefinition,
    FieldDefinition (..),
    FieldsDefinition,
    FieldContent (..),
    DirectivesDefinition,
    DirectiveDefinition (..),
    Directives,
    Directive (..),
    fieldVisibility,
    renderArgumentValues,
    renderDirectives,
    fieldArguments,
    lookupDeprecation,
  )
where

import Data.Mergeable
  ( IsMap (lookup),
    NameCollision (..),
    OrdMap,
  )
import Data.Mergeable.Utils
  ( Empty (..),
    KeyOf (..),
    selectOr,
  )
import Instances.TH.Lift ()
import Language.Haskell.TH.Syntax (Lift (..))
import Language.Iris.Rendering.RenderGQL
  ( RenderGQL (..),
    Rendering,
    intercalate,
    renderArguments,
    renderEntry,
    renderObject,
    space,
  )
import Language.Iris.Types.Internal.AST.Base
  ( Description,
    Position,
  )
import Language.Iris.Types.Internal.AST.Directive (DirectiveLocation)
import Language.Iris.Types.Internal.AST.Error
  ( GQLError,
    at,
    msg,
  )
import Language.Iris.Types.Internal.AST.Name
  ( FieldName,
    isNotSystemName,
  )
import Language.Iris.Types.Internal.AST.Role
  ( DATA_TYPE,
    RESOLVER_TYPE,
    Role,
    ToAny (..),
    toAny,
  )
import Language.Iris.Types.Internal.AST.Stage
  ( Stage,
  )
import Language.Iris.Types.Internal.AST.Type
  ( Nullable (..),
    TypeRef (..),
  )
import Language.Iris.Types.Internal.AST.Value
  ( ScalarValue (..),
    Value (..),
  )
import Relude hiding (empty, intercalate)

data Argument (valid :: Stage) = Argument
  { argumentPosition :: Position,
    argumentName :: FieldName,
    argumentValue :: Value valid
  }
  deriving (Show, Eq, Lift)

instance KeyOf FieldName (Argument stage) where
  keyOf = argumentName

instance RenderGQL (Argument s) where
  renderGQL Argument {argumentName, argumentValue} =
    renderEntry argumentName argumentValue

instance NameCollision GQLError (Argument s) where
  nameCollision Argument {argumentName, argumentPosition} =
    ("There can Be only One Argument Named " <> msg argumentName)
      `at` argumentPosition

type Arguments (s :: Stage) = OrdMap FieldName (Argument s)

renderArgumentValues :: Arguments s -> Rendering
renderArgumentValues = renderArguments . filter notNull . toList
  where
    notNull Argument {argumentValue = Null} = False
    notNull _ = True

data Directive (s :: Stage) = Directive
  { directivePosition :: Position,
    directiveName :: FieldName,
    directiveArgs :: Arguments s
  }
  deriving (Show, Lift, Eq)

instance NameCollision GQLError (Directive s) where
  nameCollision Directive {directiveName} =
    "The directive "
      <> msg ("@" <> directiveName)
      <> " can only be used once at his location."

instance KeyOf FieldName (Directive s) where
  keyOf = directiveName

instance RenderGQL (Directive s) where
  renderGQL Directive {..} =
    "@" <> renderGQL directiveName
      <> renderArgumentValues directiveArgs

type Directives s = OrdMap FieldName (Directive s)

renderDirectives :: Directives s -> Rendering
renderDirectives xs
  | null dirs = ""
  | otherwise = space <> intercalate space (fmap renderGQL dirs)
  where
    dirs = filter notSystem (toList xs)
    notSystem Directive {directiveName = "include"} = False
    notSystem Directive {directiveName = "skip"} = False
    notSystem _ = True

data DirectiveDefinition s = DirectiveDefinition
  { directiveDefinitionName :: FieldName,
    directiveDefinitionDescription :: Maybe Description,
    directiveDefinitionArgs :: ArgumentsDefinition s,
    directiveDefinitionLocations :: NonEmpty DirectiveLocation
  }
  deriving (Show, Lift)

instance NameCollision GQLError (DirectiveDefinition s) where
  nameCollision DirectiveDefinition {directiveDefinitionName} =
    "There can Be only One DirectiveDefinition Named "
      <> msg directiveDefinitionName
      <> "."

type DirectivesDefinition s = OrdMap FieldName (DirectiveDefinition s)

instance KeyOf FieldName (DirectiveDefinition s) where
  keyOf = directiveDefinitionName

instance ToAny FieldDefinition where
  toAny FieldDefinition {fieldContent, ..} = FieldDefinition {fieldContent = toAny <$> fieldContent, ..}

instance ToAny FieldContent where
  toAny (ResolverFieldContent x) = ResolverFieldContent x
  toAny DataFieldContent = DataFieldContent

type FieldsDefinition cat s = OrdMap FieldName (FieldDefinition cat s)

data FieldDefinition (cat :: Role) (s :: Stage) = FieldDefinition
  { fieldDescription :: Maybe Description,
    fieldName :: FieldName,
    fieldContent :: Maybe (FieldContent cat s),
    fieldType :: TypeRef,
    fieldDirectives :: Directives s
  }
  deriving (Show, Lift, Eq)

data FieldContent (cat :: Role) (s :: Stage) where
  DataFieldContent :: FieldContent cat s
  ResolverFieldContent ::
    { fieldArgumentsDefinition :: ArgumentsDefinition s
    } ->
    FieldContent RESOLVER_TYPE s

fieldArguments :: FieldDefinition c s -> ArgumentsDefinition s
fieldArguments FieldDefinition {fieldContent = Just (ResolverFieldContent args)} = args
fieldArguments _ = empty

deriving instance Eq (FieldContent cat s)

deriving instance Show (FieldContent cat s)

deriving instance Lift (FieldContent cat s)

instance KeyOf FieldName (FieldDefinition cat s) where
  keyOf = fieldName

instance NameCollision GQLError (FieldDefinition cat s) where
  nameCollision FieldDefinition {fieldName} =
    "There can Be only One field Named " <> msg fieldName

instance RenderGQL (FieldDefinition cat s) where
  renderGQL FieldDefinition {fieldName, fieldType, fieldContent = Just (ResolverFieldContent args)} =
    renderGQL fieldName <> renderGQL args <> ": " <> renderGQL fieldType
  renderGQL FieldDefinition {fieldName, fieldType} =
    renderEntry fieldName fieldType

instance RenderGQL (FieldsDefinition cat s) where
  renderGQL = renderObject . filter fieldVisibility . toList

instance Nullable (FieldDefinition cat s) where
  isNullable = isNullable . fieldType

fieldVisibility :: FieldDefinition cat s -> Bool
fieldVisibility = isNotSystemName . fieldName

type ArgumentsDefinition s = OrdMap FieldName (ArgumentDefinition s)

instance RenderGQL (ArgumentsDefinition s) where
  renderGQL = renderArguments . toList

instance RenderGQL (ArgumentDefinition s) where
  renderGQL = renderGQL . argument

data ArgumentDefinition s = ArgumentDefinition
  { argument :: FieldDefinition DATA_TYPE s,
    argumentDefaultValue :: Maybe (Value s)
  }
  deriving (Show, Lift, Eq)

instance KeyOf FieldName (ArgumentDefinition s) where
  keyOf = keyOf . argument

instance NameCollision GQLError (ArgumentDefinition s) where
  nameCollision ArgumentDefinition {argument} =
    "There can Be only One argument Named " <> msg (fieldName argument)

lookupDeprecation :: Directives s -> Maybe Description
lookupDeprecation = fmap readReason . lookup "deprecated"

readReason :: Directive s -> Description
readReason = selectOr "" argumentStringValue "reason" . directiveArgs

argumentStringValue :: Argument s -> Description
argumentStringValue Argument {argumentValue = Null} = ""
argumentStringValue Argument {argumentValue = (Scalar (String x))} = x
argumentStringValue _ = "can't read deprecated Reason Value"
