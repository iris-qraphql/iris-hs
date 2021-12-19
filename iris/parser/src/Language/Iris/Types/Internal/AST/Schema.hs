{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Iris.Types.Internal.AST.Schema
  ( mkSchema,
    typeDefinitions,
    lookupDataType,
    Schema (..),
    constraintResolverVariant,
  )
where

import Control.Monad.Except (MonadError (throwError))
import qualified Data.HashMap.Lazy as HM
import Data.Mergeable
  ( Merge (..),
  )
import Data.Mergeable.SafeHashMap
  ( toHashMap,
  )
import Data.Mergeable.Utils
  ( IsMap (..),
    empty,
    fromElems,
    lookup,
    toPair,
  )
import Instances.TH.Lift ()
import Language.Haskell.TH.Syntax (Lift (..))
import Language.Iris.Rendering.RenderGQL
  ( RenderGQL (..),
    intercalate,
    newline,
  )
import Language.Iris.Types.Internal.AST.Error
  ( GQLError,
    msg,
  )
import Language.Iris.Types.Internal.AST.Fields
  ( DirectivesDefinition,
  )
import Language.Iris.Types.Internal.AST.Name
  ( TypeName,
    isNotSystemTypeName,
    unpackVariantTypeName,
  )
import Language.Iris.Types.Internal.AST.Role
  ( RESOLVER_TYPE,
    ToRESOLVER (..),
    toRESOLVER,
  )
import Language.Iris.Types.Internal.AST.Stage
  ( Stage,
  )
import Language.Iris.Types.Internal.AST.TypeSystem
  ( (<:>),
    ListDefinitions,
    TypeContent (..),
    TypeDefinition (..),
    TypeDefinitions,
  )
import Language.Iris.Types.Internal.AST.Variant
  ( Variant (..),
  )
import Relude hiding
  ( empty,
    intercalate,
    show,
  )

data Schema (s :: Stage) = Schema
  { types :: TypeDefinitions s,
    query :: TypeDefinition RESOLVER_TYPE s,
    mutation :: Maybe (TypeDefinition RESOLVER_TYPE s),
    subscription :: Maybe (TypeDefinition RESOLVER_TYPE s),
    lists :: ListDefinitions,
    directiveDefinitions :: DirectivesDefinition s
  }
  deriving (Show, Lift)

instance (MonadError GQLError m) => Merge m (Schema s) where
  merge s1 s2 =
    Schema
      <$> merge (types s1) (types s2)
      <*> mergeOperation (query s1) (query s2)
      <*> mergeOptional (mutation s1) (mutation s2)
      <*> mergeOptional (subscription s1) (subscription s2)
      <*> merge (lists s1) (lists s2)
      <*> directiveDefinitions s1 <:> directiveDefinitions s2

instance RenderGQL (Schema s) where
  renderGQL schema@Schema {..} =
    intercalate newline (fmap renderGQL visibleTypes)
    where
      visibleTypes =
        filter
          (isNotSystemTypeName . typeName)
          (sort $ toList types)
          <> rootTypeDefinitions schema

typeDefinitions :: Schema s -> HashMap TypeName (TypeDefinition RESOLVER_TYPE s)
typeDefinitions schema@Schema {..} = toHashMap types <> HM.fromList (map toPair $ rootTypeDefinitions schema)

rootTypeDefinitions :: Schema s -> [TypeDefinition RESOLVER_TYPE s]
rootTypeDefinitions Schema {..} = map toRESOLVER $ catMaybes [Just query, mutation, subscription]

lookupDataType :: MonadError GQLError m => TypeName -> Schema s -> m (TypeDefinition RESOLVER_TYPE s)
lookupDataType name Schema {types, query, mutation, subscription} =
  maybe
    -- TODO: use type class unknown
    (throwError $ "Unknown type " <> msg name <> ".")
    pure
    ( isType name query
        <|> (mutation >>= isType name)
        <|> (subscription >>= isType name)
        <|> lookup (fst (unpackVariantTypeName name)) types
    )

isType :: TypeName -> TypeDefinition RESOLVER_TYPE s -> Maybe (TypeDefinition RESOLVER_TYPE s)
isType name x
  | name == typeName x = pure (toRESOLVER x)
  | otherwise = Nothing

mkSchema :: MonadError GQLError m => [TypeDefinition RESOLVER_TYPE s] -> DirectivesDefinition s -> m (Schema s)
mkSchema ts directiveDefinitions = do
  typeMap <- fromElems ts
  Schema
    (foldr delete typeMap ["Query", "Mutation", "Subscription"])
    <$> (lookupOperationType "Query" typeMap >>= maybe (throwError "Query root type must be provided.") pure)
    <*> lookupOperationType "Mutation" typeMap
    <*> lookupOperationType "Subscription" typeMap
    <*> pure empty -- LISTS
    <*> pure directiveDefinitions

lookupOperationType ::
  (MonadError GQLError m) =>
  TypeName ->
  TypeDefinitions s ->
  m (Maybe (TypeDefinition RESOLVER_TYPE s))
lookupOperationType name types = case lookup name types of
  Just t -> constraintResolverVariant t $> Just t
  _ -> pure Nothing

constraintResolverVariant :: MonadError GQLError m => TypeDefinition a s -> m (Variant RESOLVER_TYPE s)
constraintResolverVariant TypeDefinition {typeContent = ResolverTypeContent _ (v :| [])} = pure v
constraintResolverVariant TypeDefinition {typeName} = throwError $ msg typeName <> " type must be an object if provided!"

mergeOptional ::
  (Monad m, MonadError GQLError m) =>
  Maybe (TypeDefinition RESOLVER_TYPE s) ->
  Maybe (TypeDefinition RESOLVER_TYPE s) ->
  m (Maybe (TypeDefinition RESOLVER_TYPE s))
mergeOptional Nothing y = pure y
mergeOptional (Just x) Nothing = pure (Just x)
mergeOptional (Just x) (Just y) = Just <$> mergeOperation x y

mergeOperation ::
  (Monad m, MonadError GQLError m) =>
  TypeDefinition RESOLVER_TYPE s ->
  TypeDefinition RESOLVER_TYPE s ->
  m (TypeDefinition RESOLVER_TYPE s)
mergeOperation
  TypeDefinition {typeContent = ResolverTypeContent Nothing (v1 :| [])}
  TypeDefinition {typeContent = ResolverTypeContent Nothing (v2 :| []), ..} = do
    fields <- merge (memberFields v1) (memberFields v2)
    pure $ TypeDefinition {typeContent = ResolverTypeContent Nothing ((v1 {memberFields = fields}) :| []), ..}
mergeOperation TypeDefinition {} TypeDefinition {} = throwError "can't merge non object types"
