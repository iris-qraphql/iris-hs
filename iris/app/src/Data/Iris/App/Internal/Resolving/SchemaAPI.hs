{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Iris.App.Internal.Resolving.SchemaAPI
  ( schemaAPI,
  )
where

import Data.Iris.App.Internal.Resolving.Resolver (Resolver, withArguments)
import Data.Iris.App.Internal.Resolving.Types
  ( ObjectTypeResolver (..),
    ResolverValue,
    mkList,
    mkNull,
    mkObject,
  )
import Data.Iris.App.RenderIntrospection
  ( mkObjectType,
    render,
  )
import Data.Mergeable.Utils
  ( empty,
    selectOr,
  )
import Language.Iris.Types.Internal.AST
  ( Argument (..),
    FieldName,
    LAZY,
    QUERY,
    ScalarValue (..),
    Schema (..),
    TypeDefinition (..),
    TypeName,
    VALID,
    Value (..),
    packName,
    typeDefinitions,
  )
import Relude hiding (empty)
import qualified Relude as HM

resolveTypes :: Monad m => Schema VALID -> m (ResolverValue m)
resolveTypes schema = mkList <$> traverse render (toList $ typeDefinitions schema)

renderOperation ::
  Monad m =>
  Maybe (TypeDefinition LAZY VALID) ->
  m (ResolverValue m)
renderOperation (Just TypeDefinition {typeName}) = pure $ mkObjectType typeName Nothing empty
renderOperation Nothing = pure mkNull

findType ::
  Monad m =>
  TypeName ->
  Schema VALID ->
  m (ResolverValue m)
findType name = selectOr (pure mkNull) render name . typeDefinitions

schemaResolver :: Monad m => Schema VALID -> m (ResolverValue m)
schemaResolver schema@Schema {query, mutation, subscription, directiveDefinitions} =
  pure $
    mkObject
      (Just "__Schema")
      [ ("types", resolveTypes schema),
        ("queryType", renderOperation (Just query)),
        ("mutationType", renderOperation mutation),
        ("subscriptionType", renderOperation subscription),
        ("directives", render $ toList directiveDefinitions)
      ]

schemaAPI :: Monad m => Schema VALID -> ObjectTypeResolver (Resolver QUERY e m)
schemaAPI schema =
  ObjectTypeResolver
    ( HM.fromList
        [ ("__type", withArguments typeResolver),
          ("__schema", schemaResolver schema)
        ]
    )
  where
    typeResolver = selectOr (pure mkNull) handleArg ("name" :: FieldName)
      where
        handleArg
          Argument
            { argumentValue = (Scalar (String typename))
            } = findType (packName typename) schema
        handleArg _ = pure mkNull
