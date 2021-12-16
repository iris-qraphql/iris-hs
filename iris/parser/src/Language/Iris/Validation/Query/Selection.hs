{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Iris.Validation.Query.Selection
  ( validateOperation,
    validateFragmentSelection,
  )
where

import Control.Monad.Except (throwError)
import Data.Mergeable
  ( toNonEmpty,
  )
import Data.Mergeable.Utils
  ( Empty (..),
    keyOf,
    singleton,
    throwErrors,
  )
import Language.Iris.Error.Selection
  ( hasNoSubfields,
    subfieldsNotSelected,
  )
import Language.Iris.Types.Internal.AST
  ( Arguments,
    DirectiveLocation (FIELD, FRAGMENT_SPREAD, INLINE_FRAGMENT, MUTATION, QUERY, SUBSCRIPTION),
    Directives,
    FieldDefinition (..),
    FieldName,
    Fragment (..),
    FragmentName,
    GQLError,
    RESOLVER_TYPE,
    Operation (..),
    OperationType (..),
    RAW,
    Ref (..),
    Selection (..),
    SelectionContent (..),
    SelectionSet,
    TypeContent (..),
    TypeDefinition (..),
    UnionMember (..),
    UnionTag (..),
    VALID,
    at,
    isLeaf,
    mergeNonEmpty,
    mkTypeRef,
    msg,
    typed,
    __typename,
  )
import Language.Iris.Types.Internal.Validation
  ( FragmentValidator,
    SelectionValidator,
    askType,
    getOperationType,
    selectKnown,
    setSelection,
    withScope,
  )
import Language.Iris.Validation.Internal.Arguments
  ( validateFieldArguments,
  )
import Language.Iris.Validation.Internal.Directive
  ( shouldIncludeSelection,
    validateDirectives,
  )
import Language.Iris.Validation.Query.Fragment
  ( ValidateFragmentSelection,
    selectFragmentType,
    validateFragment,
    validateSpread,
  )
import Language.Iris.Validation.Query.UnionSelection
  ( validateUnionSelection,
  )
import Relude hiding (empty, join)

selectionsWithoutTypename :: SelectionSet VALID -> [Selection VALID]
selectionsWithoutTypename = filter ((__typename /=) . keyOf) . toList

singleTopLevelSelection :: Operation RAW -> SelectionSet VALID -> SelectionValidator ()
singleTopLevelSelection Operation {operationType = Subscription, operationName} selSet =
  case selectionsWithoutTypename selSet of
    (_ : (x : xs)) -> throwErrors $ fmap (singleTopLevelSelectionError operationName) (x :| xs)
    _ -> pure ()
singleTopLevelSelection _ _ = pure ()

singleTopLevelSelectionError :: Maybe FieldName -> Selection VALID -> GQLError
singleTopLevelSelectionError name Selection {selectionPosition} =
  ( maybe "Anonymous Subscription" (("Subscription " <>) . msg) name
      <> " must select "
      <> "only one top level field."
  )
    `at` selectionPosition

validateOperation ::
  Operation RAW ->
  SelectionValidator (Operation VALID)
validateOperation
  rawOperation@Operation
    { operationName,
      operationType,
      operationSelection,
      operationDirectives,
      ..
    } =
    do
      typeDef <- getOperationType rawOperation
      selection <- validateSelectionSet typeDef operationSelection
      singleTopLevelSelection rawOperation selection
      directives <-
        validateDirectives
          (toDirectiveLocation operationType)
          operationDirectives
      pure $
        Operation
          { operationName,
            operationType,
            operationArguments = empty,
            operationSelection = selection,
            operationDirectives = directives,
            ..
          }

toDirectiveLocation :: OperationType -> DirectiveLocation
toDirectiveLocation Subscription = SUBSCRIPTION
toDirectiveLocation Mutation = MUTATION
toDirectiveLocation Query = QUERY

processSelectionDirectives ::
  DirectiveLocation ->
  Directives RAW ->
  (Directives VALID -> FragmentValidator s (SelectionSet VALID)) ->
  FragmentValidator s (Maybe (SelectionSet VALID))
processSelectionDirectives location rawDirectives sel = do
  directives <- validateDirectives location rawDirectives
  include <- shouldIncludeSelection directives
  selection <- sel directives
  pure $
    if include
      then Just selection
      else Nothing

validateFragmentSelection :: (ValidateFragmentSelection s) => Fragment RAW -> FragmentValidator s (SelectionSet VALID)
validateFragmentSelection f@Fragment {fragmentSelection} = do
  typeDef <- selectFragmentType f
  validateSelectionSet typeDef fragmentSelection

validateSelectionSet ::
  (ValidateFragmentSelection s) =>
  UnionMember RESOLVER_TYPE VALID ->
  SelectionSet RAW ->
  FragmentValidator s (SelectionSet VALID)
validateSelectionSet typeDef =
  traverse (validateSelection typeDef) . toList
    >=> toNonEmpty . catMaybes
    >=> mergeNonEmpty

-- validate single selection: InlineFragments and Spreads will Be resolved and included in SelectionSet
validateSelection :: ValidateFragmentSelection s => UnionMember RESOLVER_TYPE VALID -> Selection RAW -> FragmentValidator s (Maybe (SelectionSet VALID))
validateSelection typeDef sel@Selection {..} =
  withScope (setSelection (memberName typeDef) selectionRef) $
    processSelectionDirectives FIELD selectionDirectives validateContent
  where
    selectionRef = Ref selectionName selectionPosition
    validateContent directives = do
      (validArgs, content) <- validateSelectionContent typeDef selectionRef selectionArguments selectionContent
      pure $
        singleton
          (keyOf sel)
          ( Selection
              { selectionArguments = validArgs,
                selectionDirectives = directives,
                selectionContent = content,
                ..
              }
          )
validateSelection typeDef (Spread dirs ref) =
  processSelectionDirectives FRAGMENT_SPREAD dirs $
    const $
      validateSpreadSelection typeDef ref
validateSelection typeDef (InlineFragment fragment@Fragment {fragmentDirectives}) =
  processSelectionDirectives INLINE_FRAGMENT fragmentDirectives $
    const $
      validateInlineFragmentSelection typeDef fragment

validateSpreadSelection ::
  ValidateFragmentSelection s =>
  UnionMember a VALID ->
  Ref FragmentName ->
  FragmentValidator s (SelectionSet VALID)
validateSpreadSelection typeDef =
  fmap unionTagSelection . validateSpread validateFragmentSelection [memberName typeDef]

validateInlineFragmentSelection ::
  ValidateFragmentSelection s =>
  UnionMember RESOLVER_TYPE VALID ->
  Fragment RAW ->
  FragmentValidator s (SelectionSet VALID)
validateInlineFragmentSelection typeDef =
  fmap fragmentSelection . validateFragment validateFragmentSelection [memberName typeDef]

selectSelectionField ::
  Ref FieldName ->
  UnionMember RESOLVER_TYPE s ->
  FragmentValidator s' (FieldDefinition RESOLVER_TYPE s)
selectSelectionField ref UnionMember {memberFields}
  | refName ref == "__typename" =
    pure
      FieldDefinition
        { fieldDescription = Nothing,
          fieldName = "__typename",
          fieldType = mkTypeRef "String",
          fieldContent = Nothing,
          fieldDirectives = empty
        }
  | otherwise = selectKnown ref memberFields

validateSelectionContent ::
  ValidateFragmentSelection s =>
  UnionMember RESOLVER_TYPE VALID ->
  Ref FieldName ->
  Arguments RAW ->
  SelectionContent RAW ->
  FragmentValidator s (Arguments VALID, SelectionContent VALID)
validateSelectionContent typeDef ref selectionArguments content = do
  fieldDef <- selectSelectionField ref typeDef
  fieldTypeDef <- askType (typed fieldType fieldDef)
  validArgs <- validateFieldArguments fieldDef selectionArguments
  validContent <- validateContent fieldTypeDef content
  pure (validArgs, validContent)
  where
    validateContent fieldTypeDef SelectionField = validateContentLeaf ref fieldTypeDef
    validateContent fieldTypeDef (SelectionSet rawSelectionSet) = validateByTypeContent fieldTypeDef ref rawSelectionSet

validateContentLeaf ::
  Ref FieldName ->
  TypeDefinition RESOLVER_TYPE VALID ->
  FragmentValidator s' (SelectionContent s)
validateContentLeaf
  (Ref selectionName selectionPosition)
  TypeDefinition {typeName, typeContent}
    | isLeaf typeContent = pure SelectionField
    | otherwise =
      throwError $ subfieldsNotSelected selectionName typeName selectionPosition

validateByTypeContent ::
  forall s.
  (ValidateFragmentSelection s) =>
  TypeDefinition RESOLVER_TYPE VALID ->
  Ref FieldName ->
  SelectionSet RAW ->
  FragmentValidator s (SelectionContent VALID)
validateByTypeContent
  TypeDefinition {typeContent, ..}
  currentSelectionRef =
    withScope (setSelection typeName currentSelectionRef)
      . __validate typeContent
    where
      __validate ::
        TypeContent RESOLVER_TYPE VALID ->
        SelectionSet RAW ->
        FragmentValidator s (SelectionContent VALID)
      -- Validate UnionSelection
      __validate ResolverTypeContent {resolverVariants = (resolverVariant :| [])} =
        fmap SelectionSet . validateSelectionSet resolverVariant
      __validate ResolverTypeContent {resolverTypeGuard, resolverVariants} =
        validateUnionSelection
          validateFragmentSelection
          validateSelectionSet
          resolverTypeGuard
          resolverVariants
      __validate _ =
        const $
          throwError $
            hasNoSubfields
              currentSelectionRef
              typeName
