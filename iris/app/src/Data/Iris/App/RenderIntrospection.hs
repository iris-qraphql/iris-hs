{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Iris.App.RenderIntrospection
  ( render,
    mkObjectType,
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
    ArgumentsDefinition,
    Description,
    DirectiveDefinition (..),
    DirectiveLocation,
    Directives,
    FieldContent (..),
    FieldDefinition (..),
    FieldName,
    FieldsDefinition,
    Name,
    RESOLVER_TYPE,
    TypeContent (..),
    TypeDefinition (..),
    TypeName,
    TypeRef (..),
    TypeWrapper (BaseType, TypeList),
    VALID,
    Value (..),
    Variant (..),
    Variants,
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
    pure $
      mkObject
        (Just "__Directive")
        [ renderName directiveDefinitionName,
          description directiveDefinitionDescription,
          ("locations", render (toList directiveDefinitionLocations)),
          ("args", render directiveDefinitionArgs)
        ]

instance RenderIntrospection DirectiveLocation where
  render locations = pure $ mkString (pack $ show locations)

instance RenderIntrospection (TypeDefinition cat VALID) where
  render
    TypeDefinition
      { typeName,
        typeDescription,
        typeContent
      } = pure $ renderContent typeContent
      where
        renderContent ScalarTypeContent {} = mkType "__Type.Scalar" typeName typeDescription []
        renderContent (DataTypeContent variants) =
          mkUnionType "__Type.ADT" "DATA" typeName typeDescription Nothing variants
        renderContent (ResolverTypeContent typeGuard variants) =
          mkUnionType "__Type.ADT" "RESOLVER" typeName typeDescription typeGuard variants

instance RenderIntrospection (FieldContent a VALID) where
  render (ResolverFieldContent args) = render args
  render _ = pure ""

instance RenderIntrospection (Value VALID) where
  render Null = pure mkNull
  render x = pure $ mkString $ fromLBS $ GQL.render x

instance RenderIntrospection (FieldDefinition a VALID) where
  render FieldDefinition {..} =
    pure $
      mkObject
        (Just "__Field")
        [ renderName fieldName,
          description fieldDescription,
          renderType fieldType,
          ("args", maybe (pure $ mkList []) render fieldContent),
          renderDeprecated fieldDirectives
        ]

instance RenderIntrospection (ArgumentDefinition VALID) where
  render ArgumentDefinition {argument = FieldDefinition {..}} =
    pure $
      mkObject
        (Just "__Argument")
        [ renderName fieldName,
          description fieldDescription,
          renderType fieldType,
          ("defaultValue", render fieldContent)
        ]

instance RenderIntrospection (ArgumentsDefinition VALID) where
  render = fmap mkList . traverse render . toList

instance RenderIntrospection TypeRef where
  render TypeRef {typeConName, typeWrappers} = pure $ renderWrapper typeWrappers
    where
      renderWrapper :: (Monad m) => TypeWrapper -> ResolverValue m
      renderWrapper (TypeList nextWrapper isNonNull) =
        __TypeRef "List" isNonNull (Just $ renderWrapper nextWrapper)
      renderWrapper (BaseType isNonNull) =
        __TypeRef typeConName isNonNull Nothing

__TypeRef :: Monad m => TypeName -> Bool -> Maybe (ResolverValue m) -> ResolverValue m
__TypeRef name isRequired value =
  mkObject
    (Just "__TypeRef")
    [ renderName name,
      ("required", render isRequired),
      renderParameters value
    ]

renderParameters :: Monad m => Maybe (ResolverValue m) -> (FieldName, m (ResolverValue m))
renderParameters value = ("parameters", pure $ mkList (maybeToList value))

renderDeprecated :: Monad m => Directives s -> (FieldName, m (ResolverValue m))
renderDeprecated dirs =
  ( "deprecation",
    render (lookupDeprecation dirs)
  )

description :: Monad m => Maybe Description -> (FieldName, m (ResolverValue m))
description = ("description",) . render

mkType ::
  (Monad m) =>
  TypeName ->
  TypeName ->
  Maybe Description ->
  [(FieldName, m (ResolverValue m))] ->
  ResolverValue m
mkType __type name desc etc =
  mkObject
    (Just __type)
    ( [ renderName name,
        description desc
      ]
        <> etc
    )

mkFieldsType ::
  (RenderIntrospection (FieldDefinition t VALID), Monad m) =>
  TypeName ->
  TypeName ->
  TypeName ->
  Maybe Description ->
  FieldsDefinition t VALID ->
  ResolverValue m
mkFieldsType kind role name desc fields =
  mkVariants
    kind
    role
    name
    desc
    Nothing
    [ ( Nothing,
        name,
        Just $
          render $
            filter fieldVisibility $
              toList fields
      )
    ]

mkUnionType ::
  (Monad m) =>
  TypeName ->
  TypeName ->
  TypeName ->
  Maybe Description ->
  Maybe TypeName ->
  Variants t VALID ->
  ResolverValue m
mkUnionType kind role name desc typeGuard variants =
  mkVariants
    kind
    role
    name
    desc
    typeGuard
    ( map
        (\x -> (if null (memberFields x) then Just name else Nothing, variantName x, Nothing))
        (toList variants)
    )

mkVariants ::
  (Monad m) =>
  TypeName ->
  TypeName ->
  TypeName ->
  Maybe Description ->
  Maybe TypeName ->
  [(Maybe TypeName, TypeName, Maybe (m (ResolverValue m)))] ->
  ResolverValue m
mkVariants
  __type
  role
  name
  desc
  typeGuard
  variants =
    mkType
      __type
      name
      desc
      [ ("role", render role),
        ("variants", pure $ mkList $ map renderVariant variants),
        ("guard", render typeGuard)
      ]

renderVariant :: Monad m => (Maybe TypeName, TypeName, Maybe (m (ResolverValue m))) -> ResolverValue m
renderVariant (namespace, variantName, fields) =
  mkObject
    (Just "__Variant")
    [ renderName variantName,
      ("namespace", render namespace),
      ("fields", fromMaybe (pure mkNull) fields)
    ]

mkObjectType ::
  Monad m =>
  TypeName ->
  Maybe Description ->
  FieldsDefinition RESOLVER_TYPE VALID ->
  ResolverValue m
mkObjectType = mkFieldsType "__Type.ADT" "RESOLVER"

renderName ::
  ( RenderIntrospection name,
    Monad m
  ) =>
  name ->
  (FieldName, m (ResolverValue m))
renderName = ("name",) . render

renderType :: Monad m => TypeRef -> (FieldName, m (ResolverValue m))
renderType = ("type",) . render
