{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Iris.Parsing.Request.Operation
  ( parseOperation,
  )
where

import Data.Mergeable.Utils
  ( empty,
  )
import Language.Iris.Parsing.Internal.Internal
  ( Parser,
    getLocation,
  )
import Language.Iris.Parsing.Internal.Pattern
  ( optionalDirectives,
    parseOperationType,
  )
import Language.Iris.Parsing.Internal.Terms
  ( colon,
    parseName,
    parseType,
    uniqTupleOpt,
    varName,
  )
import Language.Iris.Parsing.Internal.Value
  ( parseDefaultValue,
  )
import Language.Iris.Parsing.Request.Selection
  ( parseSelectionSet,
  )
import Language.Iris.Types.Internal.AST
  ( Operation (..),
    OperationType (..),
    RAW,
    Variable (..),
    VariableContent (..),
  )
import Relude hiding (empty)
import Text.Megaparsec
  ( (<?>),
    label,
  )

variableDefinition :: Parser (Variable RAW)
variableDefinition =
  label "VariableDefinition" $
    Variable
      <$> getLocation
      <*> (varName <* colon)
      <*> parseType
      <*> (DefaultValue <$> optional parseDefaultValue)

parseOperationDefinition :: Parser (Operation RAW)
parseOperationDefinition =
  label "OperationDefinition" $
    Operation
      <$> getLocation
      <*> parseOperationType
      <*> optional parseName
      <*> uniqTupleOpt variableDefinition
      <*> optionalDirectives
      <*> parseSelectionSet

parseAnonymousQuery :: Parser (Operation RAW)
parseAnonymousQuery = label "AnonymousQuery" $ do
  operationPosition <- getLocation
  operationSelection <- parseSelectionSet
  pure
    ( Operation
        { operationName = Nothing,
          operationType = Query,
          operationArguments = empty,
          operationDirectives = empty,
          ..
        }
    )
    <?> "can't parse AnonymousQuery"

parseOperation :: Parser (Operation RAW)
parseOperation = parseAnonymousQuery <|> parseOperationDefinition
