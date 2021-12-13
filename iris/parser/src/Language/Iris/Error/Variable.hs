{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Iris.Error.Variable
  ( uninitializedVariable,
    incompatibleVariableType,
  )
where

import Language.Iris.Types.Internal.AST
  ( FieldName,
    GQLError,
    Ref (..),
    TypeRef,
    Variable (..),
    at,
    msg,
  )
import Relude

incompatibleVariableType :: Ref FieldName -> Variable s -> TypeRef -> GQLError
incompatibleVariableType
  (Ref variableName argPosition)
  Variable {variableType}
  argumentType =
    ( "Variable "
        <> msg ("$" <> variableName)
        <> " of type "
        <> msg variableType
        <> " used in position expecting type "
        <> msg argumentType
        <> "."
    )
      `at` argPosition

uninitializedVariable :: Variable s -> GQLError
uninitializedVariable Variable {variableName, variableType, variablePosition} =
  ( "Variable "
      <> msg ("$" <> variableName)
      <> " of required type "
      <> msg variableType
      <> " was not provided."
  )
    `at` variablePosition
