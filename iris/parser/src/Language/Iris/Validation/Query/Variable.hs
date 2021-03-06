{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Iris.Validation.Query.Variable
  ( resolveOperationVariables,
  )
where

import Control.Monad.Except (throwError)
import Data.Mergeable
import Data.Mergeable.Utils
  ( selectOr,
  )
import Language.Iris.Error.Variable (uninitializedVariable)
import Language.Iris.Types.Internal.AST
  ( Argument (..),
    DefaultValue,
    Directive (..),
    FieldName,
    Fragment (..),
    ObjectEntry (..),
    Operation (..),
    Position,
    RAW,
    RawValue,
    Ref (..),
    ResolvedValue,
    Selection (..),
    SelectionContent (..),
    SelectionSet,
    VALID,
    ValidValue,
    Value (..),
    Variable (..),
    VariableContent (..),
    VariableDefinitions,
    Variables, 
    TypeRef (..)
  )
import Language.Iris.Types.Internal.Config
  ( Config (..),
    VALIDATION_MODE (..),
  )
import Language.Iris.Types.Internal.Validation
  ( BaseValidator,
    InputSource (..),
    askFragments,
    checkUnused,
    selectKnown,
    setPosition,
    startInput,
    withScope,
  )
import Language.Iris.Validation.Internal.Value
  ( validateInputByTypeRef,
  )
import Relude

class ExploreRefs a where
  exploreRefs :: a -> [Ref FieldName]

instance ExploreRefs RawValue where
  exploreRefs (VariableValue ref) = [ref]
  exploreRefs (Object _ fields) = concatMap (exploreRefs . entryValue) fields
  exploreRefs (List ls) = concatMap exploreRefs ls
  exploreRefs _ = []

instance ExploreRefs (Directive RAW) where
  exploreRefs Directive {directiveArgs} = concatMap exploreRefs directiveArgs

instance ExploreRefs (Argument RAW) where
  exploreRefs = exploreRefs . argumentValue

mapSelection :: (Selection RAW -> BaseValidator [b]) -> SelectionSet RAW -> BaseValidator [b]
mapSelection f = fmap concat . traverse f

allVariableRefs :: [SelectionSet RAW] -> BaseValidator (HashMap FieldName [Position])
allVariableRefs = collect <=< fmap (map toEntry . concat) . traverse (mapSelection searchRefs)
  where
    toEntry (Ref x y) = (x, [y])
    exploreSelectionContent :: SelectionContent RAW -> BaseValidator [Ref FieldName]
    exploreSelectionContent SelectionField = pure []
    exploreSelectionContent (SelectionSet selSet) = mapSelection searchRefs selSet
    ---------------------------------------
    searchRefs :: Selection RAW -> BaseValidator [Ref FieldName]
    searchRefs Selection {selectionArguments, selectionDirectives, selectionContent} =
      ((directiveRefs <> argumentRefs) <>) <$> exploreSelectionContent selectionContent
      where
        directiveRefs = concatMap exploreRefs selectionDirectives
        argumentRefs = concatMap exploreRefs selectionArguments
    searchRefs (InlineFragment Fragment {fragmentSelection, fragmentDirectives}) =
      (concatMap exploreRefs fragmentDirectives <>)
        <$> mapSelection searchRefs fragmentSelection
    searchRefs (Spread directives reference) =
      (concatMap exploreRefs directives <>)
        <$> ( askFragments
                >>= selectKnown reference
                >>= mapSelection searchRefs
                  . fragmentSelection
            )

resolveOperationVariables ::
  Config ->
  Variables ->
  Operation RAW ->
  BaseValidator (VariableDefinitions VALID)
resolveOperationVariables
  Config {validationMode}
  root
  Operation
    { operationSelection,
      operationArguments
    } =
    checkUnusedVariables
      *> traverse (lookupAndValidateValueOnBody root validationMode) operationArguments
    where
      checkUnusedVariables :: BaseValidator ()
      checkUnusedVariables = do
        uses <- allVariableRefs [operationSelection]
        checkUnused uses (toList operationArguments)

lookupAndValidateValueOnBody ::
  Variables ->
  VALIDATION_MODE ->
  Variable RAW ->
  BaseValidator (Variable VALID)
lookupAndValidateValueOnBody
  bodyVariables
  validationMode
  var@Variable
    { variableName,
      variableType = variableType,
      variablePosition,
      variableValue = DefaultValue defaultValue
    } =
    withScope (setPosition variablePosition) $ 
    toVariable <$> checkType getVariable defaultValue
    where
      toVariable x = var {variableValue = ValidVariableValue x}
      getVariable :: Maybe ResolvedValue
      getVariable = selectOr Nothing Just variableName bodyVariables
      ------------------------------------------------------------------
      -- checkType ::
      checkType ::
        Maybe ResolvedValue ->
        DefaultValue ->
        BaseValidator ValidValue
      checkType (Just variable) Nothing = validator False variable
      checkType (Just variable) (Just defValue) = validator True defValue *> validator False variable
      checkType Nothing (Just defValue) = validator True defValue
      checkType Nothing Nothing
        | validationMode /= WITHOUT_VARIABLES && isRequired variableType =
          throwError $ uninitializedVariable var
        | otherwise =
          returnNull
        where
          returnNull :: BaseValidator ValidValue
          returnNull = selectOr (pure Null) (validator False) variableName bodyVariables
      -----------------------------------------------------------------------------------------------
      validator :: Bool -> ResolvedValue -> BaseValidator ValidValue
      validator isDefaultValue varValue =
        startInput
          (SourceVariable var isDefaultValue)
          (validateInputByTypeRef variableType varValue)
