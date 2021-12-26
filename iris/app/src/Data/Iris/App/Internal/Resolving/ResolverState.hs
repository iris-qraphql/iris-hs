{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}


module Data.Iris.App.Internal.Resolving.ResolverState
  ( ResolverContext (..),
    ResolverStateT (..),
    resolverFailureMessage,
    clearStateResolverEvents,
    ResolverState,
    toResolverStateT,
    runResolverStateT,
    runResolverStateM,
    runResolverState,
    runResolverStateValueM,
    setCurrentType,
    askFieldType,
  )
where

import Control.Monad.Except (MonadError (..))
import Control.Monad.Trans.Reader (mapReaderT)
import Data.Iris.App.Internal.Resolving.Result
import Data.Mergeable.Utils
  ( Result,
    selectOr,
  )
import Language.Iris
  ( Config (..),
    RenderGQL,
    render,
  )
import Language.Iris.Types.Internal.AST
  ( FieldDefinition (fieldType),
    FieldName,
    GQLError,
    GQLResult,
    Operation,
    RESOLVER_TYPE,
    Schema,
    Selection (..),
    TypeContent (..),
    TypeDefinition (..),
    TypeName,
    TypeRef (..),
    VALID,
    Variant (..),
    at,
    getVariantName,
    internal,
    isInternal,
    lookupDataType,
    lookupTypeVariant,
    msg,
  )
import Relude

data ResolverContext = ResolverContext
  { currentSelection :: Selection VALID,
    schema :: Schema VALID,
    operation :: Operation VALID,
    config :: Config,
    currentType :: TypeName
  }
  deriving (Show)

setCurrentType ::
  ( MonadReader ResolverContext m,
    MonadError GQLError m
  ) =>
  Maybe TypeName ->
  m a ->
  m a
setCurrentType Nothing = id
setCurrentType (Just typeRefName) = local (\ctx -> ctx {currentType = typeRefName})

askFieldType :: forall m. (MonadReader ResolverContext m, MonadError GQLError m) => FieldName -> m (Maybe TypeName)
askFieldType fieldName = asks currentType >>= resolveTypeRef
  where
    resolveTypeRef :: TypeName -> m (Maybe TypeName)
    resolveTypeRef name = do
      t <- asks schema >>= lookupDataType name
      case t of
        TypeDefinition {typeContent = ResolverTypeContent _ vs} ->
          catchError
            (fieldTypeName <$> lookupTypeVariant (getVariantName name) vs)
            (const $ pure Nothing)
        _ -> pure Nothing
      where
        fieldTypeName :: Variant RESOLVER_TYPE VALID -> Maybe TypeName
        fieldTypeName = fmap typeRefName . selectOr Nothing (Just . fieldType) fieldName . variantFields 
  
type ResolverState = ResolverStateT () Identity

runResolverStateT :: ResolverStateT e m a -> ResolverContext -> ResultT e m a
runResolverStateT = runReaderT . _runResolverStateT

runResolverStateM :: ResolverStateT e m a -> ResolverContext -> m (Result GQLError ([e], a))
runResolverStateM res = runResultT . runResolverStateT res

runResolverStateValueM :: Functor m => ResolverStateT e m a -> ResolverContext -> m (Result GQLError a)
runResolverStateValueM res = fmap (fmap snd) . runResolverStateM res

runResolverState :: ResolverState a -> ResolverContext -> GQLResult a
runResolverState res = fmap snd . runIdentity . runResolverStateM res

-- Resolver Internal State
newtype ResolverStateT event m a = ResolverStateT
  { _runResolverStateT :: ReaderT ResolverContext (ResultT event m) a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader ResolverContext
    )

instance MonadTrans (ResolverStateT e) where
  lift = ResolverStateT . lift . lift

instance (Monad m) => MonadError GQLError (ResolverStateT e m) where
  throwError err = do
    ctx <- asks id
    let f = if isInternal err then renderInternalResolverError ctx else resolverFailureMessage ctx
    ResolverStateT
      $ lift
      $ throwError
      $ f err
  catchError (ResolverStateT mx) f = ResolverStateT $ catchError mx (_runResolverStateT . f)

instance (Monad m) => PushEvents e (ResolverStateT e m) where
  pushEvents = ResolverStateT . lift . pushEvents

mapResolverState ::
  ( ResultT e m a ->
    ResultT e' m' a'
  ) ->
  ResolverStateT e m a ->
  ResolverStateT e' m' a'
mapResolverState f (ResolverStateT x) = ResolverStateT (mapReaderT f x)

toResolverStateT ::
  Applicative m =>
  ResolverState a ->
  ResolverStateT e m a
toResolverStateT = mapResolverState injectResult

injectResult ::
  (Applicative m) =>
  ResultT () Identity a ->
  ResultT e m a
injectResult (ResultT (Identity x)) =
  cleanEvents $ ResultT (pure x)

-- clear events and starts new resolver with different type of events but with same value
-- use properly. only if you know what you are doing
clearStateResolverEvents :: (Functor m) => ResolverStateT e m a -> ResolverStateT e' m a
clearStateResolverEvents = mapResolverState cleanEvents

resolverFailureMessage :: ResolverContext -> GQLError -> GQLError
resolverFailureMessage
  ctx@ResolverContext
    { currentSelection =
        Selection {selectionName, selectionPosition}
    }
  err =
    "Failure on Resolving Field "
      <> msg selectionName
      <> ": "
      <> err
      <> withInternalContext ctx `at` selectionPosition

renderInternalResolverError :: ResolverContext -> GQLError -> GQLError
renderInternalResolverError ctx@ResolverContext {currentSelection} err =
  internal $
    (err <> ". " <> msg (renderContext ctx))
      `at` selectionPosition currentSelection

withInternalContext :: ResolverContext -> GQLError
withInternalContext ResolverContext {config = Config {debug = False}} = ""
withInternalContext resCTX = renderContext resCTX

renderContext :: ResolverContext -> GQLError
renderContext
  ResolverContext
    { currentSelection,
      schema,
      operation,
      currentType
    } =
    renderSection "Current Type" currentType
      <> renderSection "Current Selection" currentSelection
      <> renderSection "OperationDefinition" operation
      <> renderSection "SchemaDefinition" schema

renderSection :: RenderGQL a => GQLError -> a -> GQLError
renderSection label content =
  "\n\n" <> label <> ":\n" <> line
    <> "\n\n"
    <> msg (render content)
    <> "\n\n"
  where
    line = stimes (50 :: Int) "-"
