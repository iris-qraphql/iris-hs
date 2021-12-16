{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Iris.Validation.Internal.Arguments
  ( validateDirectiveArguments,
    validateFieldArguments,
    ArgumentsConstraints,
    Resolve,
  )
where

import Language.Iris.Types.Internal.AST
  ( Argument (..),
    ArgumentDefinition (..),
    Arguments,
    ArgumentsDefinition,
    CONST,
    DirectiveDefinition (..),
    FieldDefinition (..),
    DATA_TYPE,
    RESOLVER_TYPE,
    ObjectEntry (..),
    Position (..),
    RAW,
    VALID,
    Value (..),
    VariableDefinitions,
    fieldArguments,
    typed,
  )
import Language.Iris.Types.Internal.Validation
  ( FragmentValidator,
    InputSource (..),
    MissingRequired,
    OperationContext,
    Scope (..),
    Validator,
    askVariables,
    asksScope,
    selectKnown,
    selectRequired,
    selectWithDefaultValue,
    setPosition,
    startInput,
    withScope,
  )
import Language.Iris.Validation.Internal.Value
  ( ValidateWithDefault,
    validateInputByTypeRef,
  )
import Relude hiding (empty)

type VariableConstraints ctx =
  ( MissingRequired (VariableDefinitions VALID) ctx
  )

type ArgumentsConstraints c schemaS valueS =
  ( Resolve Argument valueS c,
    ValidateWithDefault c schemaS schemaS,
    ValidateWithDefault c schemaS CONST
  )

validateArgument ::
  ( ValidateWithDefault ctx schemaS valueS,
    ValidateWithDefault ctx schemaS schemaS
  ) =>
  Arguments valueS ->
  ArgumentDefinition schemaS ->
  Validator schemaS ctx (Argument VALID)
validateArgument
  requestArgs
  ArgumentDefinition {argument,argumentDefaultValue} =
    selectWithDefaultValue
      (toArgument argument >=> validateArgumentValue argument)
      (validateArgumentValue argument)
      argumentDefaultValue
      argument
      requestArgs

toArgument :: FieldDefinition DATA_TYPE s -> Value schemaS -> Validator schemaStage ctx (Argument schemaS)
toArgument
  FieldDefinition {fieldName}
  value = mkArg . fromMaybe (Position 0 0) <$> asksScope position
    where
      mkArg pos = Argument pos fieldName value

validateArgumentValue ::
  (ValidateWithDefault ctx schemaS valueS) =>
  FieldDefinition DATA_TYPE schemaS ->
  Argument valueS ->
  Validator schemaS ctx (Argument VALID)
validateArgumentValue
  field
  Argument {argumentValue, ..} =
    withScope (setPosition argumentPosition) $
      startInput (SourceArgument argumentName) $
        Argument
          argumentPosition
          argumentName
          <$> validateInputByTypeRef (typed fieldType field) argumentValue

validateFieldArguments ::
  FieldDefinition RESOLVER_TYPE VALID ->
  Arguments RAW ->
  FragmentValidator s (Arguments VALID)
validateFieldArguments field =
  validateArguments
    (`selectKnown` arguments)
    arguments
  where
    arguments = fieldArguments field

validateDirectiveArguments ::
  ArgumentsConstraints ctx schemaStage valueStage =>
  DirectiveDefinition schemaStage ->
  Arguments valueStage ->
  Validator schemaStage ctx (Arguments VALID)
validateDirectiveArguments
  DirectiveDefinition
    { directiveDefinitionArgs
    } =
    validateArguments
      (`selectKnown` directiveDefinitionArgs)
      directiveDefinitionArgs

validateArguments ::
  ArgumentsConstraints ctx schemaStage s =>
  (Argument CONST -> Validator schemaStage ctx (ArgumentDefinition schemaStage)) ->
  ArgumentsDefinition schemaStage ->
  Arguments s ->
  Validator schemaStage ctx (Arguments VALID)
validateArguments checkUnknown argsDef rawArgs = do
  args <- traverse resolve rawArgs
  traverse_ checkUnknown args
    *> traverse (validateArgument args) argsDef

class Resolve f s ctx where
  resolve :: f s -> Validator schemaS ctx (f CONST)

instance VariableConstraints (OperationContext VALID s) => Resolve Argument RAW (OperationContext VALID s) where
  resolve (Argument key position val) = Argument key position <$> resolve val

instance Resolve f CONST ctx where
  resolve = pure

instance VariableConstraints (OperationContext VALID s) => Resolve Value RAW (OperationContext VALID s) where
  resolve Null = pure Null
  resolve (Scalar x) = pure $ Scalar x
  resolve (List elems) = List <$> traverse resolve elems
  resolve (Object conName fields) = Object conName <$> traverse resolve fields
  resolve (VariableValue ref) =
    askVariables
      >>= fmap (ResolvedVariable ref)
        . selectRequired ref

instance VariableConstraints (OperationContext VALID s) => Resolve ObjectEntry RAW (OperationContext VALID s) where
  resolve (ObjectEntry name value) = ObjectEntry name <$> resolve value
