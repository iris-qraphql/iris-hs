{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Iris.App.RenderIntrospection
  ( render,
  )
where

import Data.Iris.App.Internal.Resolving.Types
  ( ResolverValue,
    mkBoolean,
    mkList,
    mkNull,
    mkObject,
    mkString,
  )
import Data.Mergeable.Utils
  ( fromLBS,
  )
import Data.Text (pack)
import qualified Language.Iris as GQL
import Language.Iris.Types.Internal.AST
  ( ArgumentDefinition (..),
    Description,
    DirectiveDefinition (..),
    DirectiveLocation,
    FieldDefinition (..),
    FieldName,
    Name,
    TypeContent (..),
    TypeDefinition (..),
    TypeName,
    TypeRef (..),
    VALID,
    Value (..),
    Variant (..),
    Variants,
    fieldArguments,
    fieldVisibility,
    lookupDeprecation,
    unpackName,
  )
import Relude

class RenderIntrospection a where
  render :: (Monad m) => a -> m (ResolverValue m)

instance RenderIntrospection (Name t) where
  render = pure . mkString . unpackName

instance RenderIntrospection Description where
  render = pure . mkString

instance RenderIntrospection a => RenderIntrospection [a] where
  render ls = mkList <$> traverse render ls

instance RenderIntrospection a => RenderIntrospection (Maybe a) where
  render (Just value) = render value
  render Nothing = pure mkNull

instance RenderIntrospection Bool where
  render = pure . mkBoolean

instance RenderIntrospection (DirectiveDefinition VALID) where
  render DirectiveDefinition {..} =
    renderObject
      "__Directive"
      directiveDefinitionName
      directiveDefinitionDescription
      [ ("locations", render (toList directiveDefinitionLocations)),
        ("args", render $ toList directiveDefinitionArgs)
      ]

instance RenderIntrospection DirectiveLocation where
  render locations = pure $ mkString (pack $ show locations)

instance RenderIntrospection (TypeDefinition cat VALID) where
  render TypeDefinition {..} = renderContent typeContent
    where
      renderType t = renderObject t typeName typeDescription
      renderContent ScalarTypeContent {} = renderType "__Type.Scalar" []
      renderContent (DataTypeContent variants) =
        renderType "__Type.ADT" (renderVariants "DATA" Nothing variants)
      renderContent (ResolverTypeContent typeGuard variants) =
        renderType "__Type.ADT" (renderVariants "RESOLVER" typeGuard variants)

instance RenderIntrospection (Value VALID) where
  render Null = pure mkNull
  render x = pure $ mkString $ fromLBS $ GQL.render x

instance RenderIntrospection (FieldDefinition a VALID) where
  render field@FieldDefinition {..} =
    renderObject
      "__Field"
      fieldName
      fieldDescription
      [ ("type", render fieldType),
        ("args", render $ toList $ fieldArguments field),
        ("deprecation", render (lookupDeprecation fieldDirectives))
      ]

instance RenderIntrospection (Variant a VALID) where
  render Variant {variantDescription, membership, variantName, variantFields} =
    renderObject
      "__Variant"
      (maybe variantName (<> ("." <> variantName)) membership)
      variantDescription
      [("fields", render $ filter fieldVisibility $ toList variantFields)]

instance RenderIntrospection (ArgumentDefinition VALID) where
  render ArgumentDefinition {argument = FieldDefinition {..}, ..} =
    renderObject
      "__Argument"
      fieldName
      fieldDescription
      [ ("type", render fieldType),
        ("defaultValue", render argumentDefaultValue)
      ]

instance RenderIntrospection TypeRef where
  render (TypeRef name params isRequired) =
    renderObject
      "__TypeRef"
      name
      Nothing
      [ ("parameters", render params),
        ("required", render isRequired)
      ]

renderObject ::
  (Monad m) =>
  TypeName ->
  Name t ->
  Maybe Description ->
  [(FieldName, m (ResolverValue m))] ->
  m (ResolverValue m)
renderObject __type name desc etc =
  pure $
    mkObject
      (Just __type)
      ( [ ("name", render name),
          ("description", render desc)
        ]
          <> etc
      )

renderVariants ::
  (Monad m) =>
  TypeName ->
  Maybe TypeName ->
  Variants t VALID ->
  [(FieldName, m (ResolverValue m))]
renderVariants
  role
  typeGuard
  variants =
    [ ("role", render role),
      ("variants", render $ toList variants),
      ("guard", render typeGuard)
    ]
