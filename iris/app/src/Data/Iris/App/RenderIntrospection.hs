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
    LAZY,
    STRICT,
    TRUE,
    TypeContent (..),
    TypeDefinition (..),
    TypeKind (..),
    TypeName,
    TypeRef (..),
    TypeWrapper (BaseType, TypeList),
    UnionMember (..),
    UnionTypeDefinition,
    VALID,
    Value (..),
    fieldVisibility,
    lookupDeprecated,
    lookupDeprecatedReason,
    unpackName,
  )
import Relude

class RenderIntrospection a where
  render :: (Monad m) => a -> m (ResolverValue m)

instance RenderIntrospection TypeName where
  render = pure . mkString . unpackName

instance RenderIntrospection FieldName where
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

instance RenderIntrospection TypeKind where
  render = pure . mkString . fromLBS . GQL.render

instance RenderIntrospection (DirectiveDefinition VALID) where
  render
    DirectiveDefinition
      { directiveDefinitionName,
        directiveDefinitionDescription,
        directiveDefinitionLocations,
        directiveDefinitionArgs
      } =
      pure $
        mkObject
          (Just "__Directive")
          [ renderName directiveDefinitionName,
            description directiveDefinitionDescription,
            ("locations", render directiveDefinitionLocations),
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
        renderContent ScalarTypeContent {} = mkType SCALAR typeName typeDescription []
        renderContent (StrictUnionContent variants) =
          mkUnionType DATA typeName typeDescription Nothing variants
        renderContent (StrictTypeContent inputFields) =
          mkFieldsType DATA typeName typeDescription inputFields
        renderContent (LazyTypeContent objectFields) =
          mkFieldsType (OBJECT Nothing) typeName typeDescription objectFields
        renderContent (LazyUnionContent typeGuard variants) =
          mkUnionType UNION typeName typeDescription typeGuard variants

instance RenderIntrospection (FieldContent TRUE a VALID) where
  render (ResolverFieldContent args) = render args
  render _ = pure ""

instance RenderIntrospection (Value VALID) where
  render Null = pure mkNull
  render x = pure $ mkString $ fromLBS $ GQL.render x

instance
  RenderIntrospection
    (FieldDefinition LAZY VALID)
  where
  render FieldDefinition {..} =
    pure $
      mkObject
        (Just "__Field")
        [ renderName fieldName,
          description fieldDescription,
          type' fieldType,
          ("args", maybe (pure $ mkList []) render fieldContent),
          renderDeprecated fieldDirectives
        ]

instance RenderIntrospection (ArgumentsDefinition VALID) where
  render = fmap mkList . traverse (render . argument) . toList

instance RenderIntrospection (FieldDefinition STRICT VALID) where
  render FieldDefinition {..} =
    pure $
      mkObject
        (Just "__Argument")
        [ renderName fieldName,
          description fieldDescription,
          type' fieldType,
          ("defaultValue", render fieldContent)
        ]

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
    ( [ renderName name,
        ("required", render isRequired)
      ]
        <> maybeToList (("parameter",) . pure <$> value)
    )

renderDeprecated :: Monad m => Directives s -> (FieldName, m (ResolverValue m))
renderDeprecated dirs =
  ( "deprecation",
    render (lookupDeprecated dirs >>= lookupDeprecatedReason)
  )

description :: Monad m => Maybe Description -> (FieldName, m (ResolverValue m))
description = ("description",) . render

renderTypeName :: IsString p => TypeKind -> p
renderTypeName SCALAR = "__Type.Scalar"
renderTypeName OBJECT {} = "__Type.ADT"
renderTypeName UNION = "__Type.ADT"
renderTypeName DATA = "__Type.ADT"
renderTypeName LIST = "__Type.Collection"

mkType ::
  (Monad m) =>
  TypeKind ->
  TypeName ->
  Maybe Description ->
  [(FieldName, m (ResolverValue m))] ->
  ResolverValue m
mkType kind name desc etc =
  mkObject
    (Just $ renderTypeName kind)
    ( [ renderName name,
        description desc
      ]
        <> etc
    )

mkFieldsType ::
  (RenderIntrospection (FieldDefinition t VALID), Monad m) =>
  TypeKind ->
  TypeName ->
  Maybe Description ->
  FieldsDefinition t VALID ->
  ResolverValue m
mkFieldsType kind name desc fields =
  mkVariants
    kind
    name
    desc
    Nothing
    [ ( name,
        Just $
          render $
            filter fieldVisibility $
              toList fields
      )
    ]

mkUnionType ::
  (Monad m) =>
  TypeKind ->
  TypeName ->
  Maybe Description ->
  Maybe TypeName ->
  UnionTypeDefinition t VALID ->
  ResolverValue m
mkUnionType kind name desc typeGuard variants =
  mkVariants kind name desc typeGuard (map (\x -> (memberName x, Nothing)) (toList variants))

mkVariants ::
  (Monad m) =>
  TypeKind ->
  TypeName ->
  Maybe Description ->
  Maybe TypeName ->
  [(TypeName, Maybe (m (ResolverValue m)))] ->
  ResolverValue m
mkVariants
  kind
  name
  desc
  typeGuard
  variants =
    mkType
      kind
      name
      desc
      ( [ ("role", render ((if kind == DATA then "DATA" else "RESOLVER") :: TypeName)),
          ("variants", pure $ mkList $ map mkVariant variants)
        ]
          <> optionalField "guard" typeGuard
      )
    where
      mkVariant (variantName, fields) =
        mkObject
          (Just "__Variant")
          [ renderName variantName,
            ("namespace", if null fields then render name else pure mkNull),
            ("fields", fromMaybe (pure mkNull) fields)
          ]

optionalField :: (RenderIntrospection a, Monad m) => FieldName -> Maybe a -> [(FieldName, m (ResolverValue m))]
optionalField name v = maybeToList ((name,) . render <$> v)

mkObjectType ::
  Monad m =>
  TypeName ->
  Maybe Description ->
  FieldsDefinition LAZY VALID ->
  ResolverValue m
mkObjectType = mkFieldsType (OBJECT Nothing)

renderName ::
  ( RenderIntrospection name,
    Monad m
  ) =>
  name ->
  (FieldName, m (ResolverValue m))
renderName = ("name",) . render

type' :: Monad m => TypeRef -> (FieldName, m (ResolverValue m))
type' = ("type",) . render
