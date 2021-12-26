{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Iris.App.Internal.Resolving.Types
  ( ResolverMap,
    NamedResolver (..),
    NamedResolverResult (..),
    NamedResolverRef (..),
    ResolverValue (..),
    ObjectTypeResolver (..),
    ResolverEntry,
    mkBoolean,
    mkFloat,
    mkInt,
    mkList,
    mkNull,
    mkString,
    mkObject,
  )
where

import Control.Monad.Except (MonadError (throwError))
import qualified Data.HashMap.Lazy as HM
import Data.Mergeable.Utils (KeyOf (keyOf), Merge (..))
import GHC.Show (Show (show))
import Language.Iris.Types.Internal.AST
  ( FieldName,
    GQLError,
    ScalarValue (..),
    TypeName,
    VALID,
    ValidValue,
    Value,
    internal,
  )
import Relude hiding (show)

type ResolverMap (m :: Type -> Type) = HashMap TypeName (NamedResolver m)

data NamedResolver (m :: Type -> Type) = NamedResolver
  { resolverName :: TypeName,
    resolver :: ValidValue -> m (NamedResolverResult m)
  }

instance Show (NamedResolver m) where
  show NamedResolver {..} =
    "NamedResolver { name = " <> show resolverName <> " }"

newtype ObjectTypeResolver m = ObjectTypeResolver
  { objectFields :: HashMap FieldName (m (ResolverValue m))
  }

instance Show (ObjectTypeResolver m) where
  show _ = "ObjectTypeResolver {}"

data NamedResolverRef = NamedResolverRef
  { resolverTypeName :: TypeName,
    resolverArgument :: ValidValue
  }
  deriving (Show)

data NamedResolverResult (m :: Type -> Type)
  = NamedObjectResolver (ObjectTypeResolver m)
  | NamedUnionResolver NamedResolverRef

instance KeyOf TypeName (NamedResolver m) where
  keyOf = resolverName

data ResolverValue (m :: Type -> Type)
  = ResNull
  | ResScalar ScalarValue
  | ResList [ResolverValue m]
  | ResMap [(Value VALID, ResolverValue m)]
  | ResObject (Maybe TypeName) (ObjectTypeResolver m)
  | ResRef (m NamedResolverRef)

instance
  ( Monad m,
    Applicative f,
    MonadError GQLError m
  ) =>
  Merge f (ObjectTypeResolver m)
  where
  merge (ObjectTypeResolver x) (ObjectTypeResolver y) =
    pure $ ObjectTypeResolver (HM.unionWith mergeFields x y)
    where
      mergeFields a b = (,) <$> a <*> b >>= uncurry merge

instance Show (ResolverValue m) where
  show _ = "ResolverValue {}"

instance IsString (ResolverValue m) where
  fromString = ResScalar . fromString

instance
  ( Monad f,
    MonadError GQLError f,
    Merge f (ObjectTypeResolver m)
  ) =>
  Merge f (ResolverValue m)
  where
  merge ResNull ResNull = pure ResNull
  merge ResScalar {} x@ResScalar {} = pure x
  merge (ResObject n x) (ResObject _ y) = ResObject n <$> merge x y
  merge _ _ = throwError (internal "can't merge: incompatible resolvers")

type ResolverEntry m = (FieldName, m (ResolverValue m))

mkString :: Text -> ResolverValue m
mkString = ResScalar . String

mkFloat :: Double -> ResolverValue m
mkFloat = ResScalar . Float

mkInt :: Int -> ResolverValue m
mkInt = ResScalar . Int

mkBoolean :: Bool -> ResolverValue m
mkBoolean = ResScalar . Boolean

mkList :: [ResolverValue m] -> ResolverValue m
mkList = ResList

mkNull :: ResolverValue m
mkNull = ResNull

mkObject ::
  Maybe TypeName ->
  [ResolverEntry m] ->
  ResolverValue m
mkObject name = ResObject name . ObjectTypeResolver . HM.fromList
