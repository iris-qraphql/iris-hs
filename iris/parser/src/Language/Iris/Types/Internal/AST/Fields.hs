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
    InputFieldsDefinition,
    DirectivesDefinition,
    DirectiveDefinition (..),
    Directives,
    Directive (..),
    fieldVisibility,
    lookupDeprecated,
    lookupDeprecatedReason,
    unsafeFromFields,
    mkObjectField,
    mkField,
    renderArgumentValues,
    renderDirectives,
    fieldArguments,
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
    toPair,
    unsafeFromList,
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
import Language.Iris.Types.Internal.AST.DirectiveLocation (DirectiveLocation)
import Language.Iris.Types.Internal.AST.Error
  ( GQLError,
    at,
    msg,
  )
import Language.Iris.Types.Internal.AST.Name
  ( FieldName,
    TypeName,
    isNotSystemFieldName,
  )
import Language.Iris.Types.Internal.AST.Stage
  ( Stage,
  )
import Language.Iris.Types.Internal.AST.Type
  ( Nullable (..),
    TypeRef (..),
    TypeWrapper (..),
  )
import Language.Iris.Types.Internal.AST.TypeCategory
  ( LAZY,
    STRICT,
    ToAny (..),
    TypeCategory,
    toAny,
  )
import Language.Iris.Types.Internal.AST.Value
  ( ScalarValue (..),
    Value (..),
  )
import Relude hiding (empty, intercalate)

-- scalar
------------------------------------------------------------------
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

-- directive
------------------------------------------------------------------
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

-- instance IsMap FieldName (ArgumentDefinition s) (DirectiveDefinition s) where
--   lookup key DirectiveDefinition {directiveDefinitionArgs} = lookup key directiveDefinitionArgs

lookupDeprecated :: Directives s -> Maybe (Directive s)
lookupDeprecated = lookup "deprecated"

lookupDeprecatedReason :: Directive s -> Maybe Description
lookupDeprecatedReason Directive {directiveArgs} =
  selectOr
    Nothing
    argumentStringValue
    ("reason" :: FieldName)
    directiveArgs

argumentStringValue :: Argument s -> Maybe Description
argumentStringValue Argument {argumentValue = Null} = Nothing
argumentStringValue Argument {argumentValue = (Scalar (String x))} = Just x
argumentStringValue _ = Just "can't read deprecated Reason Value"

instance ToAny FieldDefinition where
  toAny FieldDefinition {fieldContent, ..} = FieldDefinition {fieldContent = toAny <$> fieldContent, ..}

instance ToAny FieldContent where
  toAny (ResolverFieldContent x) = ResolverFieldContent x
  toAny DataFieldContent = DataFieldContent

unsafeFromFields :: [FieldDefinition cat s] -> FieldsDefinition cat s
unsafeFromFields = unsafeFromList . fmap toPair

-- 3.6 Objects : https://graphql.github.io/graphql-spec/June2018/#sec-Objects
------------------------------------------------------------------------------
--  ObjectTypeDefinition:
--    Description(opt) type Name Directives(Const)(opt) FieldsDefinition(opt)

--  FieldsDefinition
--    { FieldDefinition(list) }
--
type FieldsDefinition cat s = OrdMap FieldName (FieldDefinition cat s)

--  FieldDefinition
--    Description(opt) Name ArgumentsDefinition(opt) : Type Directives(Const)(opt)
--
-- InputValueDefinition
--   Description(opt) Name: Type DefaultValue(opt) Directives[Const](opt)

data FieldDefinition (cat :: TypeCategory) (s :: Stage) = FieldDefinition
  { fieldDescription :: Maybe Description,
    fieldName :: FieldName,
    fieldContent :: Maybe (FieldContent cat s),
    fieldType :: TypeRef,
    fieldDirectives :: Directives s
  }
  deriving (Show, Lift, Eq)

data FieldContent (cat :: TypeCategory) (s :: Stage) where
  DataFieldContent :: FieldContent cat s
  ResolverFieldContent ::
    { fieldArgumentsDefinition :: ArgumentsDefinition s
    } ->
    FieldContent LAZY s

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
  toNullable field = field {fieldType = toNullable (fieldType field)}

fieldVisibility :: FieldDefinition cat s -> Bool
fieldVisibility = isNotSystemFieldName . fieldName

mkField ::
  Maybe (FieldContent cat s) ->
  FieldName ->
  TypeRef ->
  FieldDefinition cat s
mkField fieldContent fieldName fieldType =
  FieldDefinition
    { fieldName,
      fieldContent,
      fieldDescription = Nothing,
      fieldType,
      fieldDirectives = empty
    }

mkObjectField ::
  ArgumentsDefinition s ->
  FieldName ->
  TypeWrapper ->
  TypeName ->
  FieldDefinition LAZY s
mkObjectField args fieldName typeWrappers typeConName =
  mkField
    (Just $ ResolverFieldContent args)
    fieldName
    TypeRef {typeWrappers, typeConName}

-- 3.10 Input Objects: https://spec.graphql.org/June2018/#sec-Input-Objects
---------------------------------------------------------------------------
--- InputFieldsDefinition
-- { InputValueDefinition(list) }

type InputFieldsDefinition s = OrdMap FieldName (InputValueDefinition s)

type InputValueDefinition = FieldDefinition STRICT

-- 3.6.1 Field Arguments : https://graphql.github.io/graphql-spec/June2018/#sec-Field-Arguments
-----------------------------------------------------------------------------------------------
-- ArgumentsDefinition:
--   (InputValueDefinition(list))

type ArgumentsDefinition s = OrdMap FieldName (ArgumentDefinition s)

instance RenderGQL (ArgumentsDefinition s) where
  renderGQL = renderArguments . toList

instance RenderGQL (ArgumentDefinition s) where
  renderGQL = renderGQL . argument

data ArgumentDefinition s = ArgumentDefinition
  { argument :: FieldDefinition STRICT s,
    argumentDefaultValue :: Maybe (Value s)
  }
  deriving (Show, Lift, Eq)

instance KeyOf FieldName (ArgumentDefinition s) where
  keyOf = keyOf . argument

instance NameCollision GQLError (ArgumentDefinition s) where
  nameCollision ArgumentDefinition {argument} =
    "There can Be only One argument Named " <> msg (fieldName argument)
