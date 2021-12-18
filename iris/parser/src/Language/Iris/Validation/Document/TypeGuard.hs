{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Iris.Validation.Document.TypeGuard
  ( validateTypeGuard,
  )
where

import Control.Monad.Except (throwError)
import Data.Mergeable.Utils
  ( KeyOf (..),
    empty,
    selectOr,
  )
import Language.Iris.Error.Resolver
  ( TypeGuardError (..),
    partialImplements,
  )
import qualified Language.Iris.Types.Internal.AST as T
import Language.Iris.Types.Internal.AST
  ( ArgumentDefinition (..),
    ArgumentsDefinition,
    CONST,
    FieldContent (..),
    FieldDefinition (..),
    FieldsDefinition,
    RESOLVER_TYPE,
    Subtyping (..),
    TypeName,
    TypeRef (..),
    Variant (..),
  )
import Language.Iris.Types.Internal.Validation
  ( ValidatorContext (localContext),
  )
import Language.Iris.Types.Internal.Validation.Internal
  ( askObjectType,
    resolveTypeMember,
  )
import Language.Iris.Types.Internal.Validation.SchemaValidator
  ( Field (..),
    ON_INTERFACE,
    ON_TYPE,
    PLACE,
    SchemaValidator,
    TypeEntity (..),
    TypeSystemContext (..),
    inArgument,
    inField,
    inTypeGuard,
  )
import Relude hiding (empty, local)

validateTypeGuard ::
  [Variant RESOLVER_TYPE CONST] ->
  TypeName ->
  SchemaValidator (TypeEntity ON_TYPE) TypeName
validateTypeGuard unionTypeNames typeGuardName = do
  guardType <- askObjectType typeGuardName
  traverse (resolveTypeMember >=> hasCompatibleFields guardType) unionTypeNames
    $> typeGuardName
  where
    hasCompatibleFields :: Variant RESOLVER_TYPE CONST -> Variant RESOLVER_TYPE CONST -> SchemaValidator (TypeEntity ON_TYPE) ()
    hasCompatibleFields guardType memberType =
      inTypeGuard
        (T.variantName guardType)
        (T.variantName memberType)
        $ isCompatibleTo
          (memberFields memberType)
          (memberFields guardType)

class StructuralCompatibility a where
  type Context a :: PLACE -> Type
  type Context a = Field

  -- Object (which implements interface) -> Interface -> Validation
  isCompatibleTo :: a -> a -> SchemaValidator ((Context a) ON_INTERFACE) ()

  isCompatibleBy :: (t -> a) -> t -> t -> SchemaValidator ((Context a) ON_INTERFACE) ()
  isCompatibleBy f a b = f a `isCompatibleTo` f b

instance StructuralCompatibility (FieldsDefinition RESOLVER_TYPE s) where
  type Context (FieldsDefinition RESOLVER_TYPE s) = TypeEntity
  isCompatibleTo objFields = traverse_ checkInterfaceField
    where
      checkInterfaceField interfaceField@FieldDefinition {fieldName} =
        inField fieldName $ selectOr err (`isCompatibleTo` interfaceField) fieldName objFields
        where
          err = failImplements Missing

instance StructuralCompatibility (FieldDefinition RESOLVER_TYPE s) where
  f1 `isCompatibleTo` f2 =
    isCompatibleBy fieldType f1 f2
      *> isCompatibleBy (fieldArgs . fieldContent) f1 f2

fieldArgs :: Maybe (FieldContent  RESOLVER_TYPE s) -> ArgumentsDefinition s
fieldArgs (Just (ResolverFieldContent args)) = args
fieldArgs _ = empty

instance StructuralCompatibility (ArgumentsDefinition s) where
  subArguments `isCompatibleTo` arguments = traverse_ hasCompatibleSubArgument arguments
    where
      hasCompatibleSubArgument argument =
        inArgument (keyOf argument) $
          selectOr (failImplements Missing) (`isCompatibleTo` argument) (keyOf argument) subArguments

instance StructuralCompatibility (ArgumentDefinition s) where
  isCompatibleTo = isCompatibleBy (fieldType . argument)

instance StructuralCompatibility TypeRef where
  t1 `isCompatibleTo` t2
    | t1 `isSubtype` t2 = pure ()
    | otherwise = failImplements UnexpectedType {expectedType = t2, foundType = t1}

failImplements ::
  TypeGuardError ->
  SchemaValidator (Field ON_INTERFACE) a
failImplements err = do
  x <- asks (local . localContext)
  throwError $ partialImplements x err
