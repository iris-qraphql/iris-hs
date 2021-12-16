{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Iris.Validation.Query.Validation
  ( validateRequest,
  )
where

import Data.Mergeable.Utils (empty)
import Language.Iris.Types.Internal.AST
  ( ExecutableDocument (..),
    GQLResult,
    Operation (..),
    Schema (..),
    TypeKind (..),
    VALID,
    mkBaseType,
  )
import Language.Iris.Types.Internal.Config (Config (..))
import Language.Iris.Types.Internal.Validation
  ( OperationContext (..),
    Scope (..),
    ScopeKind (..),
    runValidator,
  )
import Language.Iris.Validation.Query.Fragment
  ( validateFragments,
  )
import Language.Iris.Validation.Query.FragmentPreconditions
  ( checkFragmentPreconditions,
  )
import Language.Iris.Validation.Query.Selection
  ( validateFragmentSelection,
    validateOperation,
  )
import Language.Iris.Validation.Query.Variable
  ( resolveOperationVariables,
  )
import Relude hiding
  ( empty,
    fromList,
  )

validateRequest ::
  Config ->
  Schema VALID ->
  ExecutableDocument ->
  GQLResult (Operation VALID)
validateRequest
  config
  schema
  ExecutableDocument
    { fragments,
      inputVariables,
      operation =
        operation@Operation
          { operationName,
            operationSelection,
            operationPosition
          }
    } =
    do
      variables <-
        runValidator
          validateHelpers
          config
          schema
          scope
          ( OperationContext
              { operationName,
                fragments,
                variables = empty
              }
          )
      validFragments <-
        runValidator
          (validateFragments validateFragmentSelection)
          config
          schema
          scope
          ( OperationContext
              { operationName,
                fragments,
                variables
              }
          )
      runValidator
        (validateOperation operation)
        config
        schema
        scope
        ( OperationContext
            { operationName,
              fragments = validFragments,
              variables
            }
        )
    where
      scope =
        Scope
          { kind = SELECTION,
            currentTypeName = "Root",
            currentTypeKind = RESOLVER Nothing,
            currentTypeWrappers = mkBaseType,
            fieldName = "Root",
            position = Just operationPosition,
            path = []
          }
      validateHelpers =
        checkFragmentPreconditions operationSelection
          *> resolveOperationVariables
            config
            inputVariables
            operation
