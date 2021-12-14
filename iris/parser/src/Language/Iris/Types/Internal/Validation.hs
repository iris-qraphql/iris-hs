{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Iris.Types.Internal.Validation
  ( Validator,
    SelectionValidator,
    InputValidator,
    BaseValidator,
    InputSource (..),
    OperationContext (..),
    runValidator,
    askType,
    selectRequired,
    selectKnown,
    Constraint (..),
    constraint,
    asksScope,
    selectWithDefaultValue,
    startInput,
    inField,
    inputMessagePrefix,
    checkUnused,
    Prop (..),
    ScopeKind (..),
    setDirective,
    inputValueSource,
    askVariables,
    Scope (..),
    MissingRequired (..),
    InputContext,
    Unknown,
    askFragments,
    getOperationType,
    selectType,
    FragmentValidator,
    withScope,
    setPosition,
    setSelection,
    ValidatorContext (..),
    askObjectType,
  )
where

-- Resolution,

import Control.Monad.Except (throwError)
import Data.Mergeable.Utils
  ( IsMap,
    KeyOf (..),
    member,
    selectBy,
    selectOr,
    throwErrors,
  )
import Language.Iris.Error.Class
  ( KindViolation (..),
    MissingRequired (..),
    Unknown (..),
    Unused (..),
  )
import Language.Iris.Types.Internal.AST
  ( FieldDefinition (..),
    FieldName,
    LAZY,
    Position (..),
    Ref (..),
    STRICT,
    TypeName,
    Value (..),
    fromAny,
    isNullable,
    withPath,
  )
import Language.Iris.Types.Internal.AST.TypeSystem
import Language.Iris.Types.Internal.Validation.Internal
  ( askObjectType,
    askType,
    getOperationType,
  )
import Language.Iris.Types.Internal.Validation.Validator
import Relude hiding (Constraint)

getUnused :: (KeyOf k b, IsMap k c, Foldable t) => c a -> t b -> [b]
getUnused uses = filter (not . (`member` uses) . keyOf) . toList

failOnUnused :: Unused a => [a] -> Validator s (OperationContext s1 s2) ()
failOnUnused [] = pure ()
failOnUnused (x : xs) = do
  ctx <- Validator ask
  throwErrors $ (`withPath` path (scope ctx)) . unused (localContext ctx) <$> (x :| xs)

checkUnused ::
  ( KeyOf k b,
    IsMap k c,
    Unused b,
    Foldable t
  ) =>
  c a ->
  t b ->
  Validator s (OperationContext s1 s2) ()
checkUnused uses = failOnUnused . getUnused uses

constraint ::
  KindViolation k inp =>
  Constraint (k :: TypeCategory) ->
  inp ->
  TypeDefinition LAZY s ->
  Validator s ctx (TypeDefinition k s)
constraint ONLY_OBJECT _ TypeDefinition {typeContent = LazyTypeContent {lazyObjectFields}, ..} =
  pure TypeDefinition {typeContent = LazyTypeContent {lazyObjectFields}, ..}
constraint ONLY_DATA ctx x = maybe (throwError (kindViolation ONLY_DATA ctx)) pure (fromAny x)
constraint target ctx _ = throwError (kindViolation target ctx)

selectRequired ::
  ( IsMap FieldName c,
    MissingRequired (c a) ctx
  ) =>
  Ref FieldName ->
  c a ->
  Validator s ctx a
selectRequired selector container =
  do
    ValidatorContext {scope, localContext} <- Validator ask
    selectBy
      (missingRequired scope localContext selector container)
      (keyOf selector)
      container

selectWithDefaultValue ::
  forall ctx c s validValue a.
  ( IsMap FieldName c,
    MissingRequired (c a) ctx
  ) =>
  (Value s -> Validator s ctx validValue) ->
  (a -> Validator s ctx validValue) ->
  Maybe (Value s) ->
  FieldDefinition STRICT s ->
  c a ->
  Validator s ctx validValue
selectWithDefaultValue
  f
  validateF
  defaultValue
  field@FieldDefinition {fieldName}
  values =
    selectOr
      (handleNull defaultValue)
      validateF
      fieldName
      values
    where
      ------------------
      handleNull :: Maybe (Value s) -> Validator s ctx validValue
      handleNull (Just value) = f value
      handleNull Nothing
        | isNullable field = f Null
        | otherwise = failSelection
      -----------------
      failSelection = do
        ValidatorContext {scope, localContext} <- Validator ask
        position <- asksScope position
        throwError $ missingRequired scope localContext (Ref fieldName (fromMaybe (Position 0 0) position)) values

selectType ::
  TypeName ->
  Validator s ctx (TypeDefinition LAZY s)
selectType name = do
  ValidatorContext {scope, localContext, schema} <- Validator ask
  maybe
    (throwError $ unknown scope localContext name)
    pure
    (lookupDataType name schema)

selectKnown ::
  ( IsMap k c,
    Unknown sel ctx,
    KeyOf k sel
  ) =>
  sel ->
  c a ->
  Validator s ctx a
selectKnown selector lib =
  do
    ValidatorContext {scope, localContext} <- Validator ask
    selectBy
      (unknown scope localContext selector)
      (keyOf selector)
      lib
