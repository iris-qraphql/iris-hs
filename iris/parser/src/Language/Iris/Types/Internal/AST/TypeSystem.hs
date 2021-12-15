{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Iris.Types.Internal.AST.TypeSystem
  ( ScalarDefinition (..),
    StrictUnionContent,
    UnionTypeDefinition,
    TypeContent (..),
    TypeDefinition (..),
    Schema (..),
    TypeDefinitions,
    TypeCategory,
    mkType,
    createScalarType,
    initTypeLib,
    kindOf,
    isLeaf,
    lookupWith,
    RawTypeDefinition (..),
    RootOperationTypeDefinition (..),
    SchemaDefinition (..),
    buildSchema,
    Typed (Typed),
    untyped,
    typed,
    defineSchemaWith,
    typeDefinitions,
    lookupDataType,
    HistoryT,
    startHistory,
    (<:>),
    mergeNonEmpty,
  )
where

import Control.Monad.Except (MonadError (throwError))
import qualified Data.HashMap.Lazy as HM
import Data.Mergeable
  ( Merge (..),
    NameCollision (..),
    OrdMap,
    mergeConcat,
  )
import Data.Mergeable.SafeHashMap
  ( SafeHashMap,
    toHashMap,
  )
import Data.Mergeable.Utils
  ( Empty (..),
    IsMap (..),
    KeyOf (..),
    insert,
    selectOr,
    toPair,
  )
import qualified Data.Text as T
import Instances.TH.Lift ()
import Language.Haskell.TH.Syntax (Lift (..))
import Language.Iris.Rendering.RenderGQL
  ( RenderGQL (..),
    Rendering,
    intercalate,
    newline,
    renderEntry,
    renderMembers,
    renderObject,
  )
import Language.Iris.Types.Internal.AST.Base
  ( Description,
    Ref,
    TRUE,
    Token,
  )
import Language.Iris.Types.Internal.AST.Error
  ( GQLError,
    msg,
  )
import Language.Iris.Types.Internal.AST.Fields
  ( DirectiveDefinition (..),
    Directives,
    DirectivesDefinition,
  )
import Language.Iris.Types.Internal.AST.Name
  ( FieldName,
    TypeName,
    isNotSystemTypeName,
    unpackName,
    unpackVariantTypeName,
  )
import Language.Iris.Types.Internal.AST.OperationType
  ( OperationType (..),
    toOperationType,
  )
import Language.Iris.Types.Internal.AST.Stage
  ( CONST,
    Stage,
    VALID,
  )
import Language.Iris.Types.Internal.AST.Type
  ( Strictness (..),
    TypeKind (..),
  )
import Language.Iris.Types.Internal.AST.TypeCategory
  ( FromAny (..),
    IS_OBJECT,
    LAZY,
    STRICT,
    ToAny (..),
    TypeCategory,
    fromAny,
    toAny,
    type (<=!),
    type (<=?),
  )
import Language.Iris.Types.Internal.AST.Union
  ( UnionMember (memberFields),
    UnionTypeDefinition,
  )
import Language.Iris.Types.Internal.AST.Value
  ( Value (..),
  )
import Relude hiding
  ( empty,
    intercalate,
    show,
  )
import Prelude (Show (..))

mergeNonEmpty :: (Merge (HistoryT m) a, MonadError e m) => NonEmpty a -> m a
mergeNonEmpty = startHistory . mergeConcat

startHistory :: HistoryT m a -> m a
startHistory m = runReaderT m []

type HistoryT = ReaderT [Ref FieldName]

(<:>) :: (Merge (HistoryT m) a, Monad m) => a -> a -> m a
x <:> y = startHistory (merge x y)

type StrictUnionContent k s = [UnionMember k s]

-- used for preserving type information from untyped values
-- e.g
-- unionType :: UnionMember STRICT VALID -> Typed STRICT VALID TypeName
-- unionType = typed memberName
typed :: (a c s -> b) -> a c s -> Typed c s b
typed f = Typed . f

untyped :: (a -> b) -> Typed c s a -> b
untyped f = f . _untyped

-- | used for preserving type information from untyped values
-- see function typed
newtype Typed (cat :: TypeCategory) (s :: Stage) a = Typed
  { _untyped :: a
  }

-- scalar
------------------------------------------------------------------
newtype ScalarDefinition = ScalarDefinition
  {validateValue :: Value VALID -> Either Token (Value VALID)}

instance Eq ScalarDefinition where
  _ == _ = False

instance Show ScalarDefinition where
  show _ = "ScalarDefinition"

instance Lift ScalarDefinition where
  lift _ = [|ScalarDefinition pure|]

#if MIN_VERSION_template_haskell(2,16,0)
  liftTyped _ = [||ScalarDefinition pure||]
#endif

-- 3.2 Schema : https://graphql.github.io/graphql-spec/June2018/#sec-Schema
---------------------------------------------------------------------------
-- SchemaDefinition :
--    schema Directives[Const](opt) { RootOperationTypeDefinition(list)}
--
-- RootOperationTypeDefinition :
--    OperationType: NamedType

data Schema (s :: Stage) = Schema
  { types :: TypeDefinitions s,
    query :: TypeDefinition (IS_OBJECT LAZY) s,
    mutation :: Maybe (TypeDefinition (IS_OBJECT LAZY) s),
    subscription :: Maybe (TypeDefinition (IS_OBJECT LAZY) s),
    directiveDefinitions :: DirectivesDefinition s
  }
  deriving (Show, Lift)

instance
  ( Monad m,
    MonadError GQLError m
  ) =>
  Merge m (Schema s)
  where
  merge s1 s2 =
    Schema
      <$> merge (types s1) (types s2)
      <*> mergeOperation (query s1) (query s2)
      <*> mergeOptional (mutation s1) (mutation s2)
      <*> mergeOptional (subscription s1) (subscription s2)
      <*> directiveDefinitions s1 <:> directiveDefinitions s2

mergeOptional ::
  (Monad m, MonadError GQLError m) =>
  Maybe (TypeDefinition (IS_OBJECT LAZY) s) ->
  Maybe (TypeDefinition (IS_OBJECT LAZY) s) ->
  m (Maybe (TypeDefinition (IS_OBJECT LAZY) s))
mergeOptional Nothing y = pure y
mergeOptional (Just x) Nothing = pure (Just x)
mergeOptional (Just x) (Just y) = Just <$> mergeOperation x y

mergeOperation ::
  (Monad m, MonadError GQLError m) =>
  TypeDefinition (IS_OBJECT LAZY) s ->
  TypeDefinition (IS_OBJECT LAZY) s ->
  m (TypeDefinition (IS_OBJECT LAZY) s)
mergeOperation
  TypeDefinition {typeContent = LazyTypeContent fields1}
  TypeDefinition {typeContent = LazyTypeContent fields2, ..} =
    do
      fields <- merge (memberFields fields1) (memberFields fields2)
      pure $ TypeDefinition {typeContent = LazyTypeContent (fields1 {memberFields = fields}), ..}

data SchemaDefinition = SchemaDefinition
  { schemaDirectives :: Directives CONST,
    unSchemaDefinition :: OrdMap OperationType RootOperationTypeDefinition
  }
  deriving (Show)

instance RenderGQL SchemaDefinition where
  renderGQL = renderSchemaDefinition . toList . unSchemaDefinition

renderSchemaDefinition :: [RootOperationTypeDefinition] -> Rendering
renderSchemaDefinition entries = "schema" <> renderObject entries <> newline

instance NameCollision GQLError SchemaDefinition where
  nameCollision _ = "There can Be only One SchemaDefinition."

instance KeyOf TypeName SchemaDefinition where
  keyOf _ = "schema"

data RawTypeDefinition
  = RawSchemaDefinition SchemaDefinition
  | RawTypeDefinition (TypeDefinition LAZY CONST)
  | RawDirectiveDefinition (DirectiveDefinition CONST)
  deriving (Show)

data RootOperationTypeDefinition = RootOperationTypeDefinition
  { rootOperationType :: OperationType,
    rootOperationTypeDefinitionName :: TypeName
  }
  deriving (Show, Eq)

instance NameCollision GQLError RootOperationTypeDefinition where
  nameCollision RootOperationTypeDefinition {rootOperationType} =
    "There can Be only One TypeDefinition for schema." <> msg rootOperationType

instance KeyOf OperationType RootOperationTypeDefinition where
  keyOf = rootOperationType

instance RenderGQL RootOperationTypeDefinition where
  renderGQL
    RootOperationTypeDefinition
      { rootOperationType,
        rootOperationTypeDefinitionName
      } = renderEntry rootOperationType rootOperationTypeDefinitionName

type TypeDefinitions s = SafeHashMap TypeName (TypeDefinition LAZY s)

typeDefinitions :: Schema s -> HashMap TypeName (TypeDefinition LAZY s)
typeDefinitions schema@Schema {..} = toHashMap types <> HM.fromList operations
  where
    operations = map toPair $ rootTypeDefinitions schema

rootTypeDefinitions :: Schema s -> [TypeDefinition LAZY s]
rootTypeDefinitions Schema {..} = map toAny $ catMaybes [Just query, mutation, subscription]

mkSchema :: (Monad m, MonadError GQLError m) => [TypeDefinition LAZY s] -> m (Schema s)
mkSchema types =
  traverse3
    (popByKey types)
    ( RootOperationTypeDefinition Query "Query",
      RootOperationTypeDefinition Mutation "Mutation",
      RootOperationTypeDefinition Subscription "Subscription"
    )
    >>= defineSchemaWith types

defineSchemaWith ::
  ( Monad f,
    MonadError GQLError f
  ) =>
  [TypeDefinition cat s] ->
  ( Maybe (TypeDefinition (IS_OBJECT LAZY) s),
    Maybe (TypeDefinition (IS_OBJECT LAZY) s),
    Maybe (TypeDefinition (IS_OBJECT LAZY) s)
  ) ->
  f (Schema s)
defineSchemaWith oTypes (Just query, mutation, subscription) = do
  let types = excludeTypes [Just query, mutation, subscription] oTypes
  let schema = (initTypeLib query) {mutation, subscription}
  foldlM (flip defineType) schema types
defineSchemaWith _ (Nothing, _, _) = throwError "Query root type must be provided."

excludeTypes :: [Maybe (TypeDefinition c1 s)] -> [TypeDefinition c2 s] -> [TypeDefinition c2 s]
excludeTypes exclusionTypes = filter ((`notElem` blacklist) . typeName)
  where
    blacklist :: [TypeName]
    blacklist = fmap typeName (catMaybes exclusionTypes)

withDirectives ::
  (Monad m, MonadError GQLError m) =>
  DirectivesDefinition s ->
  Schema s ->
  m (Schema s)
withDirectives dirs Schema {..} = do
  dirs' <- directiveDefinitions <:> dirs
  pure $
    Schema
      { directiveDefinitions = dirs',
        ..
      }

buildSchema ::
  (Monad m, MonadError GQLError m) =>
  ( Maybe SchemaDefinition,
    [TypeDefinition LAZY s],
    DirectivesDefinition s
  ) ->
  m (Schema s)
buildSchema (Nothing, types, dirs) = mkSchema types >>= withDirectives dirs
buildSchema (Just schemaDef, types, dirs) =
  traverse3 selectOp (Query, Mutation, Subscription)
    >>= defineSchemaWith types
    >>= withDirectives dirs
  where
    selectOp op = selectOperation schemaDef op types

traverse3 :: Applicative t => (a -> t b) -> (a, a, a) -> t (b, b, b)
traverse3 f (a1, a2, a3) = (,,) <$> f a1 <*> f a2 <*> f a3

typeReference ::
  (Monad m, MonadError GQLError m) =>
  [TypeDefinition LAZY s] ->
  RootOperationTypeDefinition ->
  m (Maybe (TypeDefinition (IS_OBJECT LAZY) s))
typeReference types rootOperation =
  popByKey types rootOperation
    >>= maybe
      (throwError $ "Unknown type " <> msg (rootOperationTypeDefinitionName rootOperation) <> ".")
      (pure . Just)

selectOperation ::
  ( Monad f,
    MonadError GQLError f
  ) =>
  SchemaDefinition ->
  OperationType ->
  [TypeDefinition LAZY s] ->
  f (Maybe (TypeDefinition (IS_OBJECT LAZY) s))
selectOperation SchemaDefinition {unSchemaDefinition} operationType lib =
  selectOr (pure Nothing) (typeReference lib) operationType unSchemaDefinition

initTypeLib :: TypeDefinition (IS_OBJECT LAZY) s -> Schema s
initTypeLib query =
  Schema
    { types = empty,
      query = query,
      mutation = Nothing,
      subscription = Nothing,
      directiveDefinitions = empty
    }

isType :: TypeName -> TypeDefinition (IS_OBJECT LAZY) s -> Maybe (TypeDefinition LAZY s)
isType name x
  | name == typeName x = Just (toAny x)
  | otherwise = Nothing

lookupDataType :: TypeName -> Schema s -> Maybe (TypeDefinition LAZY s)
lookupDataType name Schema {types, query, mutation, subscription} =
  isType name query
    <|> (mutation >>= isType name)
    <|> (subscription >>= isType name)
    <|> lookup (fst (unpackVariantTypeName name)) types

data TypeDefinition (a :: TypeCategory) (s :: Stage) = TypeDefinition
  { typeDescription :: Maybe Description,
    typeName :: TypeName,
    typeDirectives :: Directives s,
    typeContent :: TypeContent TRUE a s
  }
  deriving (Show, Lift, Eq)

instance Ord (TypeDefinition k s) where
  compare a b =
    compare (indexOf $ typeContent a) (indexOf $ typeContent b)
      <> compare (typeName a) (typeName b)

instance KeyOf TypeName (TypeDefinition a s) where
  keyOf = typeName

instance Strictness (TypeDefinition k s) where
  isResolverType = isResolverType . typeContent

instance NameCollision GQLError (TypeDefinition cat s) where
  nameCollision x =
    "There can Be only One TypeDefinition Named " <> msg (typeName x) <> "."

instance ToAny TypeDefinition where
  toAny TypeDefinition {typeContent, ..} = TypeDefinition {typeContent = toAny typeContent, ..}

instance
  (FromAny (TypeContent TRUE) cat) =>
  FromAny TypeDefinition cat
  where
  fromAny TypeDefinition {typeContent, ..} = bla <$> fromAny typeContent
    where
      bla x = TypeDefinition {typeContent = x, ..}

type TypeContentOfKind r a s = TypeContent (r <=? a) a s

data
  TypeContent
    (b :: Bool)
    (a :: TypeCategory)
    (s :: Stage)
  where
  ScalarTypeContent ::
    { dataScalar :: ScalarDefinition
    } ->
    TypeContentOfKind STRICT a s
  StrictTypeContent ::
    {dataVariants :: UnionTypeDefinition STRICT s} ->
    TypeContentOfKind (IS_OBJECT STRICT) a s
  LazyTypeContent ::
    { resolverVariant :: UnionMember LAZY s
    } ->
    TypeContentOfKind (IS_OBJECT LAZY) a s
  LazyUnionContent ::
    { unionTypeGuardName :: Maybe TypeName,
      unionMembers :: UnionTypeDefinition LAZY s
    } ->
    TypeContentOfKind LAZY a s

deriving instance Show (TypeContent a b s)

deriving instance Eq (TypeContent a b s)

deriving instance Lift (TypeContent a b s)

indexOf :: TypeContent b a s -> Int
indexOf ScalarTypeContent {} = 0
indexOf StrictTypeContent {} = 2
indexOf LazyTypeContent {} = 5
indexOf LazyUnionContent {} = 6

instance Strictness (TypeContent TRUE k s) where
  isResolverType LazyTypeContent {} = True
  isResolverType LazyUnionContent {} = True
  isResolverType _ = False

instance ToAny (TypeContent TRUE) where
  toAny ScalarTypeContent {..} = ScalarTypeContent {..}
  toAny StrictTypeContent {..} = StrictTypeContent {..}
  toAny LazyTypeContent {..} = LazyTypeContent {..}
  toAny LazyUnionContent {..} = LazyUnionContent {..}

instance FromAny (TypeContent TRUE) STRICT where
  fromAny ScalarTypeContent {..} = Just ScalarTypeContent {..}
  fromAny StrictTypeContent {..} = Just StrictTypeContent {..}
  fromAny _ = Nothing

instance FromAny (TypeContent TRUE) LAZY where
  fromAny ScalarTypeContent {..} = Just ScalarTypeContent {..}
  fromAny LazyTypeContent {..} = Just LazyTypeContent {..}
  fromAny LazyUnionContent {..} = Just LazyUnionContent {..}
  fromAny StrictTypeContent {..} = Just StrictTypeContent {..}

instance FromAny (TypeContent TRUE) (IS_OBJECT LAZY) where
  fromAny LazyTypeContent {..} = Just LazyTypeContent {..}
  fromAny _ = Nothing

mkType :: TypeName -> TypeContent TRUE a s -> TypeDefinition a s
mkType typeName typeContent =
  TypeDefinition
    { typeName,
      typeDescription = Nothing,
      typeDirectives = empty,
      typeContent
    }

createScalarType :: (STRICT <=! a) => TypeName -> TypeDefinition a s
createScalarType typeName = mkType typeName $ ScalarTypeContent (ScalarDefinition pure)

isLeaf :: TypeContent TRUE a s -> Bool
isLeaf ScalarTypeContent {} = True
isLeaf StrictTypeContent {} = True
isLeaf _ = False

kindOf :: TypeDefinition a s -> TypeKind
kindOf TypeDefinition {typeName, typeContent} = __kind typeContent
  where
    __kind ScalarTypeContent {} = SCALAR
    __kind StrictTypeContent {} = DATA
    __kind LazyTypeContent {} = OBJECT (toOperationType typeName)
    __kind LazyUnionContent {} = UNION

defineType ::
  ( Monad m,
    MonadError GQLError m
  ) =>
  TypeDefinition k s ->
  Schema s ->
  m (Schema s)
defineType datatype lib = updateTypes <$> insert (toAny datatype) (types lib)
  where
    updateTypes types = lib {types}

lookupWith :: Eq k => (a -> k) -> k -> [a] -> Maybe a
lookupWith f key = find ((== key) . f)

popByKey ::
  (MonadError GQLError m) =>
  [TypeDefinition LAZY s] ->
  RootOperationTypeDefinition ->
  m (Maybe (TypeDefinition (IS_OBJECT LAZY) s))
popByKey types (RootOperationTypeDefinition opType name) = case lookupWith typeName name types of
  Just dt@TypeDefinition {typeContent = LazyTypeContent {}} ->
    pure (fromAny dt)
  Just {} ->
    throwError $
      msg (show opType)
        <> " root type must be Object type if provided, it cannot be "
        <> msg name
  _ -> pure Nothing

--
-- OTHER
--------------------------------------------------------------------------------------------------

hasDefaultOperationName :: RootOperationTypeDefinition -> Bool
hasDefaultOperationName
  RootOperationTypeDefinition
    { rootOperationType,
      rootOperationTypeDefinitionName = name
    } = show rootOperationType == T.unpack (unpackName name)

instance RenderGQL (Schema s) where
  renderGQL schema@Schema {..} =
    intercalate newline (fmap renderGQL visibleTypes <> schemaDefinition)
    where
      schemaDefinition
        | all hasDefaultOperationName entries = []
        | otherwise = [renderSchemaDefinition entries]
      entries =
        catMaybes
          [ RootOperationTypeDefinition Query . typeName <$> Just query,
            RootOperationTypeDefinition Mutation . typeName <$> mutation,
            RootOperationTypeDefinition Subscription . typeName <$> subscription
          ]
      visibleTypes =
        filter
          (isNotSystemTypeName . typeName)
          (sort $ toList types)
          <> rootTypeDefinitions schema

instance RenderGQL (TypeDefinition a s) where
  renderGQL TypeDefinition {typeName, typeContent} = __render typeContent <> newline
    where
      __render ScalarTypeContent {} = "scalar " <> renderGQL typeName
      __render StrictTypeContent {dataVariants} =
        "data " <> renderGQL typeName
          <> " = "
          <> renderMembers dataVariants
      __render LazyUnionContent {unionMembers} =
        "resolver "
          <> renderGQL typeName
          <> " = "
          <> renderMembers unionMembers
      __render LazyTypeContent {resolverVariant} = "resolver " <> renderGQL typeName <> " =" <> renderGQL (memberFields resolverVariant)
